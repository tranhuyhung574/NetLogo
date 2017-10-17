// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app

import java.lang.reflect.{ Type => JType }

import org.nlogo.core.Dialect
import org.nlogo.api.{ AddableLoader, AutoConvertable, ConfigurableModelLoader, ModelLoader, Version }
import org.nlogo.fileformat, fileformat.{ ModelConversion, ModelConverter }
import org.nlogo.nvm.Workspace
import org.nlogo.workspace.{ AbstractWorkspace, CurrentModelOpener, LiveCompilerServices, ModelTracker, WorkspaceFactory }

import org.picocontainer.PicoContainer
import org.picocontainer.adapters.AbstractAdapter

import scala.collection.JavaConverters._

object Adapters {

  class ModelLoaderComponent extends AbstractAdapter[ModelLoader](classOf[ModelLoader], classOf[ConfigurableModelLoader]) {
    def getDescriptor(): String = "ModelLoaderComponent"
    def verify(container: PicoContainer): Unit = {}

    def getComponentInstance(container: PicoContainer, into: JType) = {
      val literalParser = container.getComponent(classOf[LiveCompilerServices])

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

  class ModelConverterComponent extends AbstractAdapter[ModelConversion](classOf[ModelConversion], classOf[ModelConverter]) {
    def getDescriptor(): String = "ModelConverterComponent"
    def verify(container: PicoContainer): Unit = {}

    def getComponentInstance(container: PicoContainer, into: JType) = {
      val workspace = container.getComponent(classOf[AbstractWorkspace])

      val allAutoConvertables =
        fileformat.defaultAutoConvertables ++
          container.getComponents(classOf[AutoConvertable]).asScala

      fileformat.converter(
        workspace.getExtensionManager,
        workspace.getCompilationEnvironment,
        workspace.compilerServices,
        allAutoConvertables)(
          container.getComponent(classOf[Dialect]))
    }
  }

  // we need to make HeadlessWorkspace objects for BehaviorSpace to use.
  // HeadlessWorkspace uses picocontainer too, but it could get confusing
  // to use the same container in both places, so I'm going to keep the
  // containers separate and just use Plain Old Java Reflection to
  // call HeadlessWorkspace's newInstance() method. - ST 3/11/09
  // And we'll conveniently reuse it for the preview commands editor! - NP 2015-11-18
  class AppWorkspaceFactory(modelTracker: ModelTracker, modelSaver: ModelSaver) extends WorkspaceFactory() with CurrentModelOpener {
    def currentVersion: Version = modelSaver.currentVersion
    def newInstance(is3D: Boolean): AbstractWorkspace =
      Class.forName("org.nlogo.headless.HeadlessWorkspace")
        .getMethod("newInstance")
        .invoke(is3D)
        .asInstanceOf[AbstractWorkspace]
    def openCurrentModelIn(w: Workspace): Unit = {
      w.setModelPath(modelTracker.getModelPath)
      w.openModel(modelSaver.currentModelInCurrentVersion)
    }
  }

  class WorkspaceFactoryComponent extends AbstractAdapter[WorkspaceFactory](classOf[WorkspaceFactory], classOf[AppWorkspaceFactory]) {
    def getDescriptor(): String = "WorkspaceFactoryComponent"
    def verify(container: PicoContainer): Unit = {}

    def getComponentInstance(container: PicoContainer, into: JType) = {
      val modelSaver = container.getComponent(classOf[ModelSaver])
      val modelTracker = container.getComponent(classOf[ModelTracker])
      new AppWorkspaceFactory(modelTracker, modelSaver)
    }
  }
}
