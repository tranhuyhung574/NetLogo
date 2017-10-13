// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app

import javax.swing.{ JFrame, JOptionPane, JMenu }
import java.awt.event.ActionEvent

import org.nlogo.agent.{ World2D, World3D }
import java.awt.{ Dimension, Frame, Toolkit }
import org.nlogo.api._
import org.nlogo.app.codetab.{ ExternalFileManager, TemporaryCodeTab }
import org.nlogo.app.common.{ CodeToHtml, Events => AppEvents, FileActions, FindDialog, SaveModelingCommonsAction }
import org.nlogo.app.interfacetab.{ InterfaceToolBar, WidgetPanel }
import org.nlogo.app.tools.{ AgentMonitorManager, GraphicsPreview, Preferences, PreferencesDialog, PreviewCommandsEditor }
import org.nlogo.awt.UserCancelException
import org.nlogo.core.{ AgentKind, CompilerException, Dialect, Femto, I18N,
  Shape, Widget => CoreWidget }, Shape.{ LinkShape, VectorShape }
import org.nlogo.core.model.WidgetReader
import org.nlogo.fileformat, fileformat.{ ModelConversion, ModelConverter, ScalaXmlElementFactory }
import org.nlogo.log.Logger
import org.nlogo.nvm.{ PresentationCompilerInterface, Workspace }
import org.nlogo.shape.{ LinkShapesManagerInterface, ShapesManagerInterface, TurtleShapesManagerInterface }
import org.nlogo.util.{ NullAppHandler, Pico }
import org.nlogo.window._
import org.nlogo.window.Events._
import org.nlogo.window.Event.LinkParent
import org.nlogo.workspace.{ AbstractWorkspace, Controllable, CurrentModelOpener,
  HubNetManagerFactory, ModelTracker, ModelTrackerImpl, SaveModel, SaveModelAs, WorkspaceMessageCenter, WorkspaceFactory }

import org.picocontainer.adapters.AbstractAdapter
import org.picocontainer.parameters.{ ComponentParameter, ConstantParameter }
import org.picocontainer.Parameter

import scala.io.Codec
/**
 * The main class for the complete NetLogo application.
 *
 * <p>All methods in this class, including the constructor,
 * <strong>must</strong> be called from
 * the AWT event queue thread, unless otherwise specified.
 *
 * <p>See the "Controlling" section of the NetLogo User Manual
 * for example code.
 */
object App{

  private[app] val pico = new Pico()
  // all these guys are assigned in main. yuck
  var app: App = null
  var logger: Logger = null
  // these are private[app] for refactoring purposes. Once the refactor is over they should
  // be changed back (or just passed in, instead of being gross mutable state).
  private[app] var commandLineModelIsLaunch = false
  private[app] var commandLineModel: String = null
  private[app] var commandLineMagic: String = null
  private[app] var commandLineURL: String = null
  private[app] var loggingName: String = null

  /**
   * Should be called once at startup to create the application and
   * start it running.  May not be called more than once.  Once
   * this method has called, the singleton instance of this class
   * is stored in <code>app</code>.
   *
   * <p>This method must <strong>not</strong> be called from the AWT event
   * queue thread.
   *
   * @param args Should be empty. (Passing non-empty arguments
   *             is not currently documented.)
   */
  def main(args:Array[String]){
    mainWithAppHandler(args, NullAppHandler)
  }

  def mainWithAppHandler(args: Array[String], appHandler: Object) {
    // this call is reflective to avoid complicating dependencies
    appHandler.getClass.getDeclaredMethod("init").invoke(appHandler)

    org.nlogo.window.VMCheck.detectBadJVMs()
    Logger.beQuiet()
    processCommandLineArguments(args)
    Splash.beginSplash() // also initializes AWT

    if (Version.systemDynamicsAvailable) {
      pico.add("org.nlogo.sdm.gui.NLogoGuiSDMFormat")
      pico.add(classOf[AddableLoader], "org.nlogo.sdm.gui.NLogoXGuiSDMFormat", new ConstantParameter(ScalaXmlElementFactory))
    }
    pico.addScalaObject("org.nlogo.sdm.gui.SDMGuiAutoConvertable")

    class ModelLoaderComponent extends AbstractAdapter[ModelLoader](classOf[ModelLoader], classOf[ConfigurableModelLoader]) {
      import scala.collection.JavaConverters._

      def getDescriptor(): String = "ModelLoaderComponent"
      def verify(x$1: org.picocontainer.PicoContainer): Unit = {}

      def getComponentInstance(container: org.picocontainer.PicoContainer, into: java.lang.reflect.Type) = {
        val literalParser = Femto.scalaSingleton[org.nlogo.core.LiteralParser]("org.nlogo.parse.CompilerUtilities")

        val loader =
          fileformat.standardLoader(literalParser)
        val additionalComponents =
          container.getComponents(classOf[AddableLoader]).asScala
        if (additionalComponents.nonEmpty)
          additionalComponents.foldLeft(loader) {
            case (l, component) => component.addToLoader(l)
          }
        else loader
      }
    }

    pico.addAdapter(new ModelLoaderComponent())

    class ModelConverterComponent extends AbstractAdapter[ModelConversion](classOf[ModelConversion], classOf[ModelConverter]) {
      import scala.collection.JavaConverters._

      def getDescriptor(): String = "ModelConverterComponent"
      def verify(x$1: org.picocontainer.PicoContainer): Unit = {}

      def getComponentInstance(container: org.picocontainer.PicoContainer, into: java.lang.reflect.Type) = {
        val workspace = container.getComponent(classOf[org.nlogo.workspace.AbstractWorkspace])

        val allAutoConvertables =
          fileformat.defaultAutoConvertables ++ container.getComponents(classOf[AutoConvertable]).asScala

        fileformat.converter(workspace.getExtensionManager, workspace.getCompilationEnvironment, workspace.compilerServices, allAutoConvertables)(container.getComponent(classOf[Dialect]))
      }
    }

    pico.addAdapter(new ModelConverterComponent())

    pico.addComponent(classOf[CodeToHtml])
    pico.addComponent(classOf[ModelSaver])
    pico.add("org.nlogo.gl.view.ViewManager")
    if (Version.is3D)
      pico.add("org.nlogo.gl.view.ThreeDGLViewFactory")
    else
      pico.add("org.nlogo.gl.view.TwoDGLViewFactory")
    // Anything that needs a parent Frame, we need to use ComponentParameter
    // and specify classOf[AppFrame], otherwise PicoContainer won't know which
    // Frame to use - ST 6/16/09
    pico.add(classOf[TurtleShapesManagerInterface],
          "org.nlogo.shape.editor.TurtleShapeManagerDialog",
          Array[Parameter] (
            new ComponentParameter(classOf[AppFrame]),
            new ComponentParameter(), new ComponentParameter()))
    pico.add(classOf[LinkShapesManagerInterface],
          "org.nlogo.shape.editor.LinkShapeManagerDialog",
          Array[Parameter] (
            new ComponentParameter(classOf[AppFrame]),
            new ComponentParameter(), new ComponentParameter()))
    pico.add(classOf[AggregateManagerInterface],
          "org.nlogo.sdm.gui.GUIAggregateManager",
          Array[Parameter] (
            new ComponentParameter(classOf[AppFrame]),
            new ComponentParameter(), new ComponentParameter(),
            new ComponentParameter(), new ComponentParameter()))
    pico.add("org.nlogo.lab.gui.LabManager")
    // we need to make HeadlessWorkspace objects for BehaviorSpace to use.
    // HeadlessWorkspace uses picocontainer too, but it could get confusing
    // to use the same container in both places, so I'm going to keep the
    // containers separate and just use Plain Old Java Reflection to
    // call HeadlessWorkspace's newInstance() method. - ST 3/11/09
    // And we'll conveniently reuse it for the preview commands editor! - NP 2015-11-18
    val factory = new WorkspaceFactory() with CurrentModelOpener {
      def newInstance(is3D: Boolean): AbstractWorkspace =
        Class.forName("org.nlogo.headless.HeadlessWorkspace")
          .getMethod("newInstance").invoke(is3D).asInstanceOf[AbstractWorkspace]
      def currentVersion: Version = pico.getComponent(classOf[ModelSaver]).currentVersion
      def openCurrentModelIn(w: Workspace): Unit = {
        w.setModelPath(app.workspace.getModelPath)
        w.openModel(pico.getComponent(classOf[ModelSaver]).currentModelInCurrentVersion)
      }
    }

    pico.addComponent(classOf[WorkspaceFactory], factory)
    pico.addComponent(classOf[GraphicsPreview])
    pico.addComponent(classOf[ExternalFileManager])
    pico.add(
      classOf[PreviewCommandsEditorInterface],
      "org.nlogo.app.tools.PreviewCommandsEditor",
      new ComponentParameter(classOf[AppFrame]),
      new ComponentParameter(), new ComponentParameter())
    pico.add(classOf[MenuBar], "org.nlogo.app.MenuBar")
    pico.add("org.nlogo.app.interfacetab.CommandCenter")
    pico.add("org.nlogo.app.interfacetab.InterfaceTab")
    pico.addComponent(classOf[Tabs])
    app = startConfiguredApp(true)
    // It's pretty silly, but in order for the splash screen to show up
    // for more than a fraction of a second, we want to initialize as
    // much stuff as we can from main() before handing off to the event
    // thread.  So what happens in the App constructor and what happens
    // in finishStartup() is pretty arbitrary -- it's whatever makes
    // the splash screen come up early without getting a bunch of Java
    // exceptions because we're doing too much on the main thread.
        // Hey, it's important to make a good first impression.
    //   - ST 8/19/03

    // org.nlogo.awt.EventQueue.invokeAndWait(()=>app.finishStartup(appHandler, Version.getCurrent(Version.is3D)))
    org.nlogo.awt.EventQueue.invokeAndWait(()=>app.finishStartup(appHandler, org.nlogo.api.ThreeDVersion))
  }

  private def processCommandLineArguments(args: Array[String]) {
    def printAndExit(s:String){ println(s); sys.exit(0) }
    // note: this method is static so that it can be called from main()
    // before App is instantiated, which means we can use the --version
    // flags without the AWT ever being initialized, which is handy when
    // we're at the command line and need the version but can't do GUI stuff
    // (e.g. if we're on Linux but don't have the DISPLAY environment variable
    // set) - ST 4/1/02
    var i = 0
    def nextToken() = { val t = args(i); i += 1; t }
    def moreTokens = i < args.length
    while(moreTokens){
      val token = nextToken()
      if (token == "--events") org.nlogo.window.Event.logEvents = true;
      else if (token == "--open" || token == "--launch") {
        commandLineModelIsLaunch = token == "--launch"
        require(commandLineModel == null &&
                commandLineMagic == null &&
                commandLineURL == null,
          "Error parsing command line arguments: you can only specify one model to open at startup.")
        val fileToken = nextToken()
        val modelFile = new java.io.File(fileToken)
        // Best to check if the file exists here, because after the GUI thread has started,
        // NetLogo just hangs with the splash screen showing if file doesn't exist. ~Forrest (2/12/2009)
        if (!modelFile.exists) throw new IllegalStateException(I18N.gui.getN("file.open.error.notFound", fileToken))
        commandLineModel = modelFile.getAbsolutePath()
      }
      else if (token == "--magic") {
        require(commandLineModel == null &&
                commandLineMagic == null &&
                commandLineURL == null)
        commandLineMagic = nextToken()
      }
      else if (token == "--url") {
        require(commandLineModel == null &&
                commandLineMagic == null &&
                commandLineURL == null)
        commandLineURL = nextToken()
      }
      else if (token == "--version") printAndExit(Version.version)
      else if (token == "--extension-api-version") printAndExit(APIVersion.version)
      else if (token == "--builddate") printAndExit(Version.buildDate)
      else if (token == "--logging") loggingName = nextToken()
      else if (token == "--log-directory") {
        if (logger != null) logger.changeLogDirectory(nextToken())
        else JOptionPane.showConfirmDialog(null,
          "You need to initialize the logger using the --logging options before specifying a directory.",
          "NetLogo", JOptionPane.DEFAULT_OPTION)
      }
      else if (token.startsWith("--")) {
        //TODO: Decide: should we do System.exit() here?
        // Previously we've just ignored unknown parameters, but that seems wrong to me.  ~Forrest (2/12/2009)
        System.err.println("Error: Unknown command line argument: " + token)
      }
      else { // we assume it's a filename to "launch"
        commandLineModelIsLaunch = true
        require(commandLineModel == null &&
                commandLineMagic == null &&
                commandLineURL == null,
          "Error parsing command line arguments: you can only specify one model to open at startup.")
        val modelFile = new java.io.File(token)
        // Best to check if the file exists here, because after the GUI thread has started,
        // NetLogo just hangs with the splash screen showing if file doesn't exist. ~Forrest (2/12/2009)
        if (!modelFile.exists())
          throw new IllegalStateException(I18N.gui.getN("file.open.error.notFound", token))
        commandLineModel = modelFile.getAbsolutePath()
      }
    }
  }

  def startConfiguredApp(is3D: Boolean): App = {
    val frame = new AppFrame()
    val menuBarFactory = new StatefulMenuBarFactory()
    pico.addComponent(menuBarFactory)
    val config = configureWorkspace(frame, is3D)
    new App(config, frame, menuBarFactory)
  }

  def configureWorkspace(frame: JFrame with LinkParent with LinkRoot, is3D: Boolean): WorkspaceConfig = {
    val dialect =
      if (is3D) Femto.scalaSingleton[Dialect]("org.nlogo.api.NetLogoThreeDDialect")
      else      Femto.scalaSingleton[Dialect]("org.nlogo.api.NetLogoLegacyDialect")
    val world =
      if (is3D) new World3D()
      else      new World2D()

    val messageCenter = new WorkspaceMessageCenter()
    val modelTracker = new ModelTrackerImpl(messageCenter)

    val controlSet = new AppControlSet()
    val listenerManager = new NetLogoListenerManager
    val monitorManager = new AgentMonitorManager(frame)

    // build the config
    val workspaceConfig =
      WorkspaceConfig
        .default
        .withControlSet(controlSet)
        .withExternalFileManager(pico.getComponent(classOf[ExternalFileManager]))
        .withFrame(frame)
        .withHubNetManagerFactory(pico.getComponent(classOf[HubNetManagerFactory]))
        .withKioskLevel(GUIWorkspace.KioskLevel.NONE)
        .withLinkParent(frame)
        .withListenerManager(listenerManager)
        .withMessageCenter(messageCenter)
        .withModelTracker(modelTracker)
        .withMonitorManager(monitorManager)
        .withCompiler(Femto.get[PresentationCompilerInterface]("org.nlogo.compile.Compiler", dialect))
        .withUpdateManager(new UpdateManager(world.tickCounter))
        .withWorld(world)
        .tap(config => config.withViewManager(GUIWorkspaceScala.viewManager(config.displayStatusRef)))
        .tap(config =>
            config.withOwner(new GUIJobManagerOwner(config.updateManager, config.viewManager, config.displayStatusRef, config.world, config.frame)))

    val colorizer = new EditorColorizer(workspaceConfig.compiler, workspaceConfig.extensionManager)

    pico.addComponent(frame)
    pico.addComponent(colorizer)
    pico.addComponent(workspaceConfig.compilerServices)
    pico.addComponent(new org.nlogo.properties.EditDialogFactory(workspaceConfig.compilerServices, colorizer))
    val aggregateManager = pico.getComponent(classOf[AggregateManagerInterface])

    workspaceConfig.withSourceOwner(aggregateManager)
  }

}

// TODO: Move these out into their own file

// This is for other windows to get their own copy of the menu
// bar.  It's needed especially for OS X since the screen menu bar
// doesn't get shared across windows.  -- AZS 6/17/2005
trait MenuBarFactory extends org.nlogo.window.MenuBarFactory {
  import org.nlogo.swing.UserAction, UserAction.{ ActionCategoryKey, EditCategory, FileCategory, HelpCategory, ToolsCategory }
  def actions: Seq[javax.swing.Action]

  def createMenu(newMenu: org.nlogo.swing.Menu, category: String): JMenu = {
    actions.filter(_.getValue(ActionCategoryKey) == category).foreach(newMenu.offerAction)
    newMenu
  }

  def createEditMenu:  JMenu = createMenu(new EditMenu,  EditCategory)
  def createFileMenu:  JMenu = createMenu(new FileMenu,  FileCategory)
  def createHelpMenu:  JMenu = createMenu(new HelpMenu,  HelpCategory)
  def createToolsMenu: JMenu = createMenu(new ToolsMenu, ToolsCategory)
  def createZoomMenu:  JMenu = new ZoomMenu
}

class StatefulMenuBarFactory extends MenuBarFactory {
  var actions: Seq[javax.swing.Action] = Seq()
}


class App(config: WorkspaceConfig, val frame: JFrame with LinkParent with LinkRoot, private val menuBarFactory: StatefulMenuBarFactory) extends
    org.nlogo.window.Event.LinkChild with
    org.nlogo.api.Exceptions.Handler with
    AppEvent.Handler with
    BeforeLoadEvent.Handler with
    LoadBeginEvent.Handler with
    LoadEndEvent.Handler with
    ModelSavedEvent.Handler with
    ModelSections with
    AppEvents.SwitchedTabsEvent.Handler with
    AboutToQuitEvent.Handler with
    ZoomedEvent.Handler with
    Controllable {

  val listenerManager = config.listenerManager
  val messageCenter = config.messageCenter
  val modelTracker = config.modelTracker
  val controlSet = config.controlSet

  import App.{ pico, logger, loggingName, commandLineModel, commandLineModelIsLaunch, commandLineMagic, commandLineURL }

  // all these guys get set in the locally block
  protected var _workspace: GUIWorkspace = null
  protected var _tabs: Tabs = null
  def workspace: GUIWorkspace = _workspace
  def tabs: Tabs = _tabs
  lazy val owner = new SimpleJobOwner("App", workspace.world.mainRNG, AgentKind.Observer)
  var menuBar: MenuBar = null
  var _fileManager: FileManager = null
  val monitorManager: MonitorManager = config.monitorManager
  var aggregateManager: AggregateManagerInterface = null
  var dirtyMonitor: DirtyMonitor = null
  var colorDialog: ColorDialog = null
  var labManager:LabManagerInterface = null
  var recentFilesMenu: RecentFilesMenu = null
  lazy val modelingCommons = pico.getComponent(classOf[ModelingCommonsInterface])
  private var modelSaver: ModelSaver = _
  private val ImportWorldURLProp = "netlogo.world_state_url"
  private val ImportRawWorldURLProp = "netlogo.raw_world_state_url"

  val isMac = System.getProperty("os.name").startsWith("Mac")

  def version =
    if (config.compiler.dialect.is3D) ThreeDVersion.version
    else TwoDVersion.version

  locally {
    frame.addLinkComponent(this)

    org.nlogo.swing.Utils.setSystemLookAndFeel()

    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
      def uncaughtException(t: Thread, e: Throwable) { org.nlogo.api.Exceptions.handle(e) }
    })

    val interfaceFactory = new InterfaceFactory() {
      def widgetPanel(workspace: GUIWorkspace): AbstractWidgetPanel =
        new WidgetPanel(workspace)
      def toolbar(wp: AbstractWidgetPanel, workspace: GUIWorkspace, buttons: List[WidgetInfo], frame: Frame) = {
        new InterfaceToolBar(wp.asInstanceOf[WidgetPanel], workspace, buttons, frame,
          pico.getComponent(classOf[EditDialogFactoryInterface]))
      }
    }

    frame.addLinkComponent(monitorManager)

    config.sourceOwners.foreach {
      case a: AggregateManagerInterface => aggregateManager = a
      case _ =>
    }

    pico.addComponent(classOf[ModelTracker], modelTracker)
    pico.addComponent(interfaceFactory)
    pico.addComponent(config.world)
    pico.add(classOf[HubNetManagerFactory], "org.nlogo.hubnet.server.gui.HubNetManagerFactory",
      Array[Parameter] (
        new ComponentParameter(classOf[AppFrame]), new ComponentParameter(), new ComponentParameter()))
    pico.addComponent(classOf[AgentMonitorManager], monitorManager)
    pico.add("org.nlogo.render.Renderer")

    _workspace = new GUIWorkspace(config) {
      override def newRenderer: RendererInterface = {
        val r = pico.getComponent(classOf[RendererInterface])
        if (r == null) throw new RuntimeException("Invalid Renderer!")
        r
      }
    }

    pico.addComponent(_workspace)

    frame.addLinkComponent(workspace)
    frame.addLinkComponent(new ExtensionAssistant(frame))

    _tabs = pico.getComponent(classOf[Tabs])

    config.controlSet match {
      case a: AppControlSet => a.tabs = Some(_tabs)
      case _ =>
    }

    pico.addComponent(tabs.interfaceTab.getInterfacePanel)
    frame.getContentPane.add(tabs, java.awt.BorderLayout.CENTER)
    frame.addLinkComponent(new CompilerManager(workspace, config.world, tabs.codeTab, Option(aggregateManager).toSeq))
    frame.addLinkComponent(listenerManager)

    org.nlogo.api.Exceptions.setHandler(this)
    pico.addComponent(this)

    if(loggingName != null)
      startLogging(loggingName)
  }

  /**
   * Quits NetLogo by exiting the JVM.  Asks user for confirmation first
   * if they have unsaved changes. If the user confirms, calls System.exit(0).
   */
  // part of controlling API; used by e.g. the Mathematica-NetLogo link
  // - ST 8/21/07
  @throws(classOf[UserCancelException])
  def quit(){ fileManager.quit() }

  private[app] def finishStartup(appHandler: Object, currentVersion: Version) {
    val app = pico.getComponent(classOf[App])
    val currentModelAsString = {() =>
      val modelSaver = pico.getComponent(classOf[ModelSaver])
      val is3D = Version.is3D(modelSaver.currentModel.version)
      modelSaver.modelAsString(modelSaver.currentModel, ModelReader.modelSuffix(is3D))
    }
    modelSaver = pico.getComponent(classOf[ModelSaver])
    pico.add(classOf[ModelingCommonsInterface],
          "org.nlogo.mc.ModelingCommons",
          Array[Parameter] (
            new ConstantParameter(currentModelAsString),
            new ComponentParameter(classOf[AppFrame]),
            new ConstantParameter(() => workspace.exportView),
            new ConstantParameter(() => Boolean.box(
              workspace.procedures.get("SETUP") != null &&
                workspace.procedures.get("GO") != null)),
            new ComponentParameter()))
    frame.addLinkComponent(aggregateManager)

    labManager = pico.getComponent(classOf[LabManagerInterface])
    frame.addLinkComponent(labManager)

    val titler = (file: Option[String]) => file map externalFileTitle getOrElse modelTitle
    pico.add(classOf[DirtyMonitor], "org.nlogo.app.DirtyMonitor",
      new ComponentParameter, new ComponentParameter, new ComponentParameter, new ComponentParameter,
      new ConstantParameter(titler))
    dirtyMonitor = pico.getComponent(classOf[DirtyMonitor])
    frame.addLinkComponent(dirtyMonitor)

    val menuBar = pico.getComponent(classOf[MenuBar])

    pico.add(classOf[FileManager],
      "org.nlogo.app.FileManager",
      new ComponentParameter(),
      new ConstantParameter(modelTracker),
      new ComponentParameter(), new ComponentParameter(),
      new ComponentParameter(), new ComponentParameter(),
      new ConstantParameter(menuBar), new ConstantParameter(menuBar),
      new ConstantParameter(currentVersion))
    setFileManager(pico.getComponent(classOf[FileManager]))

    val viewManager = pico.getComponent(classOf[GLViewManagerInterface])
    workspace.init(viewManager)
    frame.addLinkComponent(viewManager)

    tabs.init(fileManager, dirtyMonitor, Plugins.load(pico): _*)

    app.setMenuBar(menuBar)
    frame.setJMenuBar(menuBar)

    org.nlogo.window.RuntimeErrorDialog.init(frame)

    // OK, this is a little kludgy.  First we pack so everything
    // is realized, and all addNotify() methods are called.  But
    // the actual size we get won't be right yet, because the
    // default model hasn't been loaded.  So load it, then pack
    // again.  The first pack is needed because until everything
    // has been realized, the NetLogo event system won't work.
    //  - ST 8/16/03
    frame.pack()

    loadDefaultModel(currentVersion)
    // smartPack respects the command center's current size, rather
    // than its preferred size, so we have to explicitly set the
    // command center to the size we want - ST 1/7/05
    tabs.interfaceTab.commandCenter.setSize(tabs.interfaceTab.commandCenter.getPreferredSize)
    smartPack(frame.getPreferredSize, true)

    if (! isMac) { org.nlogo.awt.Positioning.center(frame, null) }

    org.nlogo.app.common.FindDialog.init(frame)

    Splash.endSplash()
    frame.setVisible(true)
    if(isMac){
      appHandler.getClass.getDeclaredMethod("ready", classOf[AnyRef]).invoke(appHandler, this)
    }
    menuBarFactory.actions = allActions ++ tabs.permanentMenuActions
  }

  def startLogging(properties:String) {
    if(new java.io.File(properties).exists) {
      val username =
        JOptionPane.showInputDialog(null, I18N.gui.get("tools.loggingMode.enterName"), "",
          JOptionPane.QUESTION_MESSAGE, null, null, "").asInstanceOf[String]
      if(username != null){
        logger = new Logger(username)
        listenerManager.addListener(logger)
        Logger.configure(properties)
        org.nlogo.api.Version.startLogging()
      }
    }
    else JOptionPane.showConfirmDialog(null, I18N.gui.getN("tools.loggingMode.fileDoesNotExist", properties),
      "NetLogo", JOptionPane.DEFAULT_OPTION)
  }

  ///
  private def loadDefaultModel(currentVersion: Version){
    if (commandLineModel != null) {
      if (commandLineModelIsLaunch) { // --launch through InstallAnywhere?
        // open up the blank model first so in case
        // the magic open fails for some reason
        // there's still a model loaded ev 3/7/06
        fileManager.newModel(currentVersion)
        open(commandLineModel)
      }
      else libraryOpen(commandLineModel) // --open from command line
    }
    else if (commandLineMagic != null)
      workspace.magicOpen(commandLineMagic)
    else if (commandLineURL != null) {

      try {

        fileManager.openFromURI(new java.net.URI(commandLineURL), ModelType.Library)

        import org.nlogo.awt.EventQueue

        Option(System.getProperty(ImportRawWorldURLProp)) map {
          url => // `io.Source.fromURL(url).bufferedReader` steps up to bat and... manages to fail gloriously here! --JAB (8/22/12)
            import java.io.{ BufferedReader, InputStreamReader }, java.net.URL
            EventQueue.invokeLater {
              () =>
                workspace.importWorld(new BufferedReader(new InputStreamReader(new URL(url).openStream())))
                workspace.view.dirty()
                workspace.view.repaint()
            }
        } orElse (Option(System.getProperty(ImportWorldURLProp)) map {
          url =>

            import java.util.zip.GZIPInputStream, java.io.{ ByteArrayInputStream, InputStreamReader }, scala.io.{ Codec, Source }

            val source = Source.fromURL(url)(Codec.ISO8859)
            val bytes  = source.map(_.toByte).toArray
            val bais   = new ByteArrayInputStream(bytes)
            val gis    = new GZIPInputStream(bais)
            val reader = new InputStreamReader(gis)

            EventQueue.invokeLater {
              () => {
                workspace.importWorld(reader)
                workspace.view.dirty()
                workspace.view.repaint()
                source.close()
                bais.close()
                gis.close()
                reader.close()
              }
            }

        })
      }
      catch {
        case ex: java.net.ConnectException =>
          fileManager.newModel(currentVersion)
          JOptionPane.showConfirmDialog(null,
            I18N.gui.getN("file.open.error.unloadable.message", commandLineURL),
            I18N.gui.get("file.open.error.unloadable.title"), JOptionPane.DEFAULT_OPTION)
      }

    }
    else fileManager.newModel(currentVersion)
  }

  /// zooming

  def handle(e: ZoomedEvent) {
    smartPack(frame.getPreferredSize, false)
  }

  def resetZoom() {
    new ZoomedEvent(0).raise(this)
  }

  lazy val openPreferencesDialog =
    new ShowPreferencesDialog(new PreferencesDialog(frame,
      Preferences.Language, new Preferences.LineNumbers(tabs), Preferences.IncludedFilesMenu))

  lazy val openAboutDialog = new ShowAboutWindow(frame)

  lazy val openColorDialog = new OpenColorDialog(frame)

  lazy val allActions: Seq[javax.swing.Action] = {
    val osSpecificActions = if (isMac) Seq() else Seq(openPreferencesDialog, openAboutDialog)

    val workspaceActions = org.nlogo.window.WorkspaceActions(workspace)

    val generalActions    = Seq[javax.swing.Action](
      openColorDialog,
      new ShowShapeManager("turtleShapesEditor", turtleShapesManager),
      new ShowShapeManager("linkShapesEditor",   linkShapesManager),
      new ShowSystemDynamicsModeler(aggregateManager),
      new OpenHubNetClientEditor(workspace, frame),
      workspace.hubNetControlCenterAction,
      new PreviewCommandsEditor.EditPreviewCommands(
        pico.getComponent(classOf[PreviewCommandsEditorInterface]),
        workspace,
        () => pico.getComponent(classOf[ModelSaver]).asInstanceOf[ModelSaver].currentModel),
      new SaveModelingCommonsAction(modelingCommons, frame),
      FindDialog.FIND_ACTION,
      FindDialog.FIND_NEXT_ACTION
    ) ++
    HelpActions.apply ++
    FileActions(workspace, frame) ++
    workspaceActions ++
    labManager.actions ++
    fileManager.actions

    osSpecificActions ++ generalActions
  }

  def setMenuBar(menuBar: MenuBar): Unit = {
    if (menuBar != this.menuBar) {
      this.menuBar = menuBar
      tabs.setMenu(menuBar)
      allActions.foreach(menuBar.offerAction)
      Option(recentFilesMenu).foreach(_.setMenu(menuBar))
    }
  }

  def fileManager: FileManager = _fileManager

  def setFileManager(manager: FileManager): Unit = {
    if (manager != _fileManager) {
      _fileManager = manager
      frame.addLinkComponent(manager)
      recentFilesMenu = new RecentFilesMenu(manager)
      frame.addLinkComponent(recentFilesMenu)
    }
  }

  // AppEvent stuff (kludgy)
  /**
   * Internal use only.
   */
  def handle(e:AppEvent){
    import AppEventType._
    e.`type` match {
      case RELOAD => reload()
      case MAGIC_OPEN => magicOpen(e.args(0).toString)
      case START_LOGGING =>
        startLogging(e.args(0).toString)
        if(logger!=null)
          logger.modelOpened(workspace.getModelPath)
      case ZIP_LOG_FILES =>
        if (logger==null)
          org.nlogo.log.Files.zipSessionFiles(System.getProperty("java.io.tmpdir"), e.args(0).toString)
        else
          logger.zipSessionFiles(e.args(0).toString)
      case DELETE_LOG_FILES =>
        if(logger==null)
          org.nlogo.log.Files.deleteSessionFiles(System.getProperty("java.io.tmpdir"))
        else
          logger.deleteSessionFiles()
      case _ =>
    }
  }

  private def reload() {
    val modelType = modelTracker.getModelType
    val path = modelTracker.getModelPath
    if (modelType != ModelType.New && path != null) openFromSource(FileIO.fileToString(path)(Codec.UTF8), path, modelType)
    else commandLater("print \"can't, new model\"")
  }

  private def magicOpen(name: String) {
    val matches = org.nlogo.workspace.ModelsLibrary.findModelsBySubstring(name, modelSaver.currentVersion)
    if (matches.isEmpty) commandLater("print \"no models matching \\\"" + name + "\\\" found\"")
    else {
      val fullName =
        if (matches.size == 1) matches(0)
        else {
          val options = matches.map(_.replaceAllLiterally(".nlogo3d", "").replaceAllLiterally(".nlogo", "")).toArray[AnyRef]
          val i = org.nlogo.swing.OptionDialog.showAsList(frame, I18N.gui.get("tools.magicModelMatcher"), I18N.gui.get("tools.magicModelMathcer.mustChoose"), options)
          if (i != -1) matches(i) else null
        }
      if (fullName != null) {
        org.nlogo.workspace.ModelsLibrary.getModelPath(fullName, modelSaver.currentVersion).foreach { path =>
          val source = org.nlogo.api.FileIO.fileToString(path)(Codec.UTF8)
          org.nlogo.awt.EventQueue.invokeLater(() => openFromSource(source, path, ModelType.Library))
        }
      }
    }
  }

  ///

  /**
   * Internal use only.
   */
  final def handle(e: AppEvents.SwitchedTabsEvent): Unit = {
    if (e.newTab == tabs.interfaceTab) {
      monitorManager.showAll()
      frame.toFront()
    } else if (e.oldTab == tabs.interfaceTab)
      monitorManager.hideAll()

    val title = e.newTab match {
        case tab: TemporaryCodeTab => externalFileTitle(tab.filename.merge)
        case _                     => modelTitle
      }
    frame.setTitle(title)
  }

  /**
   * Internal use only.
   */
  def handle(e: ModelSavedEvent): Unit = {
    workspace.modelSaved(e.modelPath)
    org.nlogo.window.RuntimeErrorDialog.setModelName(workspace.modelNameForDisplay)
    frame.setTitle(modelTitle)
    workspace.hubNetManager.foreach { manager =>
      manager.setTitle(modelTracker.modelNameForDisplay, modelTracker.getModelDir, modelTracker.getModelType)
    }
  }

  /**
   * Internal use only.
   */
  def handle(e: LoadBeginEvent): Unit = {
    val modelName = workspace.modelNameForDisplay
    RuntimeErrorDialog.setModelName(modelName)
    frame.setTitle(modelTitle)
    workspace.hubNetManager.foreach(_.closeClientEditor())
  }

  private var wasAtPreferredSizeBeforeLoadBegan = false
  private var preferredSizeAtLoadEndTime: java.awt.Dimension = null

  /**
   * Internal use only.
   */
  def handle(e:BeforeLoadEvent) {
    wasAtPreferredSizeBeforeLoadBegan =
            preferredSizeAtLoadEndTime == null ||
            frame.getSize == preferredSizeAtLoadEndTime ||
            frame.getSize == frame.getPreferredSize
  }

  private lazy val _turtleShapesManager: ShapesManagerInterface = {
    pico.getComponent(classOf[TurtleShapesManagerInterface])
  }
  def turtleShapesManager: ShapesManagerInterface = _turtleShapesManager

  private lazy val _linkShapesManager: ShapesManagerInterface = {
    pico.getComponent(classOf[LinkShapesManagerInterface])
  }
  def linkShapesManager: ShapesManagerInterface = _linkShapesManager

  /**
   * Internal use only.
   */
  def handle(e:LoadEndEvent){
    turtleShapesManager.reset()
    linkShapesManager.reset()
    workspace.view.repaint()

    // if we don't call revalidate() here we don't get up-to-date
    // preferred size information - ST 11/4/03
    tabs.interfaceTab.getInterfacePanel.revalidate()
    if(wasAtPreferredSizeBeforeLoadBegan) smartPack(frame.getPreferredSize, true)
    else {
      val currentSize = frame.getSize
      val preferredSize = frame.getPreferredSize
      var newWidth = currentSize.width
      if(preferredSize.width > newWidth) newWidth = preferredSize.width
      var newHeight = currentSize.height
      if(preferredSize.height > newHeight) newHeight = preferredSize.height
      if(newWidth != currentSize.width || newHeight != currentSize.height) smartPack(new Dimension(newWidth, newHeight), true)
    }
    preferredSizeAtLoadEndTime = frame.getPreferredSize()
    frame.toFront()
    tabs.interfaceTab.requestFocus()
  }

  /**
   * Internal use only.
   */
  def handle(e:AboutToQuitEvent){ if(logger != null) logger.close() }

  private def frameTitle(filename: String, dirty: Boolean) = {
    val title =
      // on OS X, use standard window title format. otherwise use Windows convention
      if(! System.getProperty("os.name").startsWith("Mac")) s"$filename - NetLogo"
      // 8212 is the unicode value for an em dash. we use the number since
      // we don't want non-ASCII characters in the source files -- AZS 6/14/2005
      else s"NetLogo ${8212.toChar} $filename"

    if (dirty) s"* $title" else title
  }

  private def modelTitle = {
    if (workspace.getModelFileName == null) "NetLogo"
    else {
      val title = frameTitle(workspace.modelNameForDisplay, dirtyMonitor.modelDirty)
      // OS X UI guidelines prohibit paths in title bars, but oh well...
      if (modelTracker.getModelType == ModelType.Normal) s"$title {${workspace.getModelDir}}" else title
    }
  }

  private def externalFileTitle(path: String) = {
    val filename = TemporaryCodeTab.stripPath(path)
    (tabs.getTabWithFilename(Right(path)) orElse tabs.getTabWithFilename(Left(path))).
      map (tab => frameTitle(filename, tab.saveNeeded)).
      getOrElse(frameTitle(filename, false))
  }

  /**
   * Internal use only.
   */
  def handle(t:Throwable): Unit = {
    try {
      val logo = t.isInstanceOf[LogoException]
      if (! logo) {
        t.printStackTrace(System.err)
        if (org.nlogo.window.RuntimeErrorDialog.suppressJavaExceptionDialogs) {
          return
        }
      }
      // The PeriodicUpdate thread may throw errors over and
      // over if a dynamic slider or other code snippet
      // generates errors.  For this reason we don't want to
      // pop up the error dialog if it is already up.
      //  -- CLB 8/15/2006
      // else we ignore it, craig originally wanted to print to stdout
      // though, the code he wrote really printed to stderr.
      // Because we redirect stderr to a file in releases this was a
      // problem.  We could write our own stack trace printer (easily)
      // that actually prints to stdout but I'm not really sure that's
      // important. ev 2/25/08
      if (!org.nlogo.window.RuntimeErrorDialog.alreadyVisible)
        org.nlogo.awt.EventQueue.invokeLater(() =>
            RuntimeErrorDialog.show(null, null, Thread.currentThread, t))
    }
    catch {
      case e2: RuntimeException => e2.printStackTrace(System.err)
    }
  }

  /// public methods for controlling (ModelCruncher uses them too)

  /**
   * Opens a model stored in a file.
   * @param path the path (absolute or relative) of the NetLogo model to open.
   */
  @throws(classOf[java.io.IOException])
  def open(path: String) {
    dispatchThreadOrBust(fileManager.openFromPath(path, ModelType.Normal))
  }

  /**
   * Saves the currently open model.
   * Should only be used by ModelResaver.
   */
  @throws(classOf[java.io.IOException])
  private[nlogo] def saveOpenModel(controller: SaveModel.Controller): Unit = {
    SaveModelAs(pico.getComponent(classOf[ModelSaver]).currentModel,
      pico.getComponent(classOf[ModelLoader]),
      controller,
      modelTracker,
      Version).foreach(thunk => thunk())
  }

  /**
   * This is called reflectively by the mac app wrapper with the full path.
   * This will only be called after appHandler.ready has been called.
   * @param path the path (absolute) to the NetLogo model to open.
   */
  def handleOpenPath(path: String) = {
    try {
      dispatchThreadOrBust {
        fileManager.aboutToCloseFiles()
        open(path)
      }
    } catch {
      case ex: UserCancelException => org.nlogo.api.Exceptions.ignore(ex)
      case ex: java.io.IOException =>
        javax.swing.JOptionPane.showMessageDialog(
          frame, ex.getMessage,
          I18N.gui.get("common.messages.error"), javax.swing.JOptionPane.ERROR_MESSAGE)
    }
  }

  /**
   * This is called reflectively by the mac app wrapper.
   */
  def handleQuit(): Unit = {
    fileManager.quit()
  }

  /**
   * This is called reflectively by the mac app wrapper.
   */
  def handleShowAbout(): Unit = {
    showAboutWindow()
  }

  /**
   * This is called reflectively by the mac app wrapper.
   */
  def handleShowPreferences(): Unit = {
    showPreferencesDialog()
  }

  @throws(classOf[java.io.IOException])
  def libraryOpen(path: String) {
    dispatchThreadOrBust(fileManager.openFromPath(path, ModelType.Library))
  }

  /**
   * Opens a model stored in a string.
   * @param name Model name (will appear in the main window's title bar)
   * @param source The complete model, including widgets and so forth,
   *               in the same format as it would be stored in a file.
   */
  def openFromSource(name:String, source:String){
    // I'm not positive that NORMAL is right here.
    openFromSource(source, name, ModelType.Normal)
  }

  def openFromSource(source:String, path:String, modelType:ModelType){
    import java.nio.file.Paths
    dispatchThreadOrBust(
      try fileManager.openFromSource(Paths.get(path).toUri, source, modelType)
      catch { case ex:UserCancelException => org.nlogo.api.Exceptions.ignore(ex) })
  }

  /**
   * Runs NetLogo commands and waits for them to complete.
   * <p>This method must <strong>not</strong> be called from the AWT event
   * queue thread or while that thread is blocked.
   * It is an error to do so.
   * @param source The command or commands to run
   * @throws org.nlogo.core.CompilerException if the code fails to compile
   * @throws IllegalStateException if called from the AWT event queue thread
   * @see #commandLater
   */
  @throws(classOf[CompilerException])
  def command(source: String) {
    org.nlogo.awt.EventQueue.cantBeEventDispatchThread()
    workspace.evaluateCommands(owner, source)
  }

  /**
   * Runs NetLogo commands in the background.  Returns immediately,
   * without waiting for the commands to finish.
   * <p>This method may be called from <em>any</em> thread.
   * @param source The command or commands to run
   * @throws org.nlogo.core.CompilerException if the code fails to compile
   * @see #command
   */
  @throws(classOf[CompilerException])
  def commandLater(source: String){
    workspace.evaluateCommands(owner, source, false)
  }

  /**
   * Runs a NetLogo reporter.
   * <p>This method must <strong>not</strong> be called from the AWT event
   * queue thread or while that thread is blocked.
   * It is an error to do so.
   * @param source The reporter to run
   * @return the result reported; may be of type java.lang.Integer, java.lang.Double,
   *         java.lang.Boolean, java.lang.String, {@link org.nlogo.core.LogoList},
   *         {@link org.nlogo.api.Agent}, AgentSet, or Nobody
   * @throws org.nlogo.core.CompilerException if the code fails to compile
   * @throws IllegalStateException if called from the AWT event queue thread
   */
  @throws(classOf[CompilerException])
  def report(source: String): Object = {
    org.nlogo.awt.EventQueue.cantBeEventDispatchThread()
    workspace.evaluateReporter(owner, source, workspace.world.observer)
  }

  /**
   * Returns the contents of the Code tab.
   * @return contents of Code tab
   */
  def getProcedures: String = dispatchThreadOrBust(tabs.codeTab.innerSource)

  /**
   * Replaces the contents of the Code tab.
   * Does not recompile the model.
   * @param source new contents
   * @see #compile
   */
  def setProcedures(source:String) { dispatchThreadOrBust(tabs.codeTab.innerSource = source) }

  /**
   * Recompiles the model.  Useful after calling
   * <code>setProcedures()</code>.
   * @see #setProcedures
   */
  def compile(){ dispatchThreadOrBust(new CompileAllEvent().raise(this)) }

  /**
   * Switches tabs.
   * @param number which tab to switch to.  0 is the Interface tab,
   *        1 the Info tab, 2 the Code tab, 3 the
   *        Errors tab.
   */
  def selectTab(number:Int){  // zero-indexed
    dispatchThreadOrBust(tabs.setSelectedIndex(number))
  }

  /**
   * Not currently supported.  For now, use <code>command</code>
   * or <code>commandLater()</code> instead.
   * @param name the button to press
   * @see #command
   * @see #commandLater
   */
  def pressButton(name:String) {
    if (java.awt.EventQueue.isDispatchThread()) throw new IllegalStateException("can't call on event thread")
    val button = findButton(name)
    if (button.forever) {
      button.foreverOn = !button.foreverOn
      org.nlogo.awt.EventQueue.invokeAndWait(() => {
        button.buttonUp = !button.foreverOn
        button.action()
      })
    }
    else {
      org.nlogo.awt.EventQueue.invokeAndWait(() => {
        button.buttonUp = false
        button.action()
      })
      while (button.running) {
        try Thread sleep 100
        catch { case ex: InterruptedException => org.nlogo.api.Exceptions.ignore(ex) }
      }
    }
  }

  /**
   * Adds new widget to Interface tab given its specification,
   * in the same (undocumented) format found in a saved model.
   * @param text the widget specification
   */
  def makeWidget(text:String) {
    dispatchThreadOrBust(
      tabs.interfaceTab.getInterfacePanel.loadWidget(
        WidgetReader.read(text.lines.toList, workspace.compilerServices, fileformat.nlogoReaders(Version.is3D))))
  }

  /// helpers for controlling methods

  private def findButton(name:String): ButtonWidget =
    tabs.interfaceTab.getInterfacePanel.getComponents
      .collect{case bw: ButtonWidget => bw}
      .find(_.displayName == name)
      .getOrElse{throw new IllegalArgumentException(
        "button '" + name + "' not found")}

  def smartPack(targetSize:Dimension, allowShrink: Boolean) {
    val gc = frame.getGraphicsConfiguration
    val maxBounds = gc.getBounds
    val insets = Toolkit.getDefaultToolkit.getScreenInsets(gc)
    val maxWidth = maxBounds.width - insets.left - insets.right
    val maxHeight = maxBounds.height - insets.top - insets.bottom
    val maxBoundsX = maxBounds.x + insets.left
    val maxBoundsY = maxBounds.y + insets.top
    val maxX = maxBoundsX + maxWidth
    val maxY = maxBoundsY + maxHeight

    tabs.interfaceTab.adjustTargetSize(targetSize)

    import StrictMath.{ max, min }

    val (currentWidth, currentHeight) = (frame.getWidth, frame.getHeight)

    // Maybe grow the window, but never shrink it
    var newWidth  = min(targetSize.width, maxWidth)
    var newHeight = min(targetSize.height, maxHeight)
    if (!allowShrink) {
      newWidth = max(newWidth, currentWidth)
      newHeight = max(newHeight, currentHeight)
    }

    // move up/left to get more room if possible and necessary
    val moveLeft = max(0, frame.getLocation().x + newWidth  - maxX)
    val moveUp   = max(0, frame.getLocation().y + newHeight - maxY)

    // now we can compute our new position
    val newX = max(maxBoundsX, frame.getLocation().x - moveLeft)
    val newY = max(maxBoundsY, frame.getLocation().y - moveUp)

    // and now that we know our position, we can compute our new size
    newWidth  = min(newWidth, maxX - newX)
    newHeight = min(newHeight, maxY - newY)

    // now do it!
    frame.setBounds(newX, newY, newWidth, newHeight)
    frame.validate()

    // not sure why this is sometimes necessary - ST 11/24/03
    tabs.requestFocus()
  }

  /**
   * Internal use only.
   */
  // used both from HelpMenu and MacHandlers - ST 2/2/09
  def showAboutWindow(): Unit = {
    openAboutDialog.actionPerformed(
      new ActionEvent(frame, ActionEvent.ACTION_PERFORMED, null))
  }

  /**
   * Internal use only.
   */
  def showPreferencesDialog(): Unit = {
    openPreferencesDialog.actionPerformed(
      new ActionEvent(frame, ActionEvent.ACTION_PERFORMED, null))
  }

  /// AppFrame
  def getLinkParent: JFrame with LinkParent with LinkRoot = frame // for Event.LinkChild

  private def dispatchThreadOrBust[T](f: => T) = {
    org.nlogo.awt.EventQueue.mustBeEventDispatchThread()
    f
  }

  def procedureSource:  String =
    tabs.codeTab.innerSource
  def widgets:          Seq[CoreWidget] = {
    tabs.interfaceTab.iP.getWidgetsForSaving
  }
  def info:             String =
    tabs.infoTab.info
  def turtleShapes:     Seq[VectorShape] =
    tabs.workspace.world.turtleShapeList.shapes.collect { case s: VectorShape => s }
  def linkShapes:       Seq[LinkShape] =
    tabs.workspace.world.linkShapeList.shapes.collect { case s: LinkShape => s }
  def additionalSections: Seq[ModelSections.ModelSaveable] = {
    val sections =
      Seq[ModelSections.ModelSaveable](tabs.workspace.previewCommands,
        labManager,
        aggregateManager,
        _workspace)
    workspace.hubNetManager.map(_ +: sections).getOrElse(sections)
  }
}
