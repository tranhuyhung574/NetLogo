package org.nlogo.tools

import java.awt.EventQueue
import java.net.URI
import java.nio.file.{ Files, FileVisitor, FileVisitResult, Path, Paths }

import scala.util.{ Failure, Success }

import org.nlogo.core.{ Femto, LiteralParser, Model }
import org.nlogo.api.{ NetLogoLegacyDialect, NetLogoThreeDDialect, Version }
import org.nlogo.app.App
import org.nlogo.workspace.{ OpenModel, OpenModelFromURI, SaveModel },
  OpenModel.{ Controller => OpenModelController },
  SaveModel.{ Controller => SaveModelController }
import org.nlogo.fileformat, fileformat.{ FailedConversionResult, NLogoFormat, NLogoXFormat, ScalaXmlElementFactory }
import org.nlogo.workspace.ModelsLibrary.modelsRoot
import org.nlogo.headless.HeadlessWorkspace
import org.nlogo.sdm.{ NLogoSDMFormat, NLogoXSDMFormat, SDMAutoConvertable }

/**
 *
 * The original motivation for that was that the version number saved in the model file
 * appears on the Modeling Commons website and it looked weird to have most model from
 * the NetLogo Model Library appear to have been saved with various older earlier versions
 * of NetLogo. Ideally, this script should be run before every release.
 *
 * This script opens and saves all models in the library, excluding 3D and HubNet models.
 * (3D and HubNet are excluded because they are not on the Modeling Commons, but if someone
 * wants to bother adding support for those models in this script, it would be a good thing.)
 *
 * The script fires up the NetLogo GUI because headless NetLogo has no saving facility.
 *
 * Nicolas 2012-10-31
 *
 * Moved to org.nlogo.tools, NP 2013-08-07
 *
 */
object ModelResaver {

  def wait(block: => Unit) {
    EventQueue.invokeAndWait(
      new Runnable() {
        def run() { block }
      })
  }

  var systemDynamicsModels: Seq[Path] = Seq()

  def main(args: Array[String]): Unit = {
    System.setProperty("org.nlogo.preferHeadless", "true")

    if (args.length > 0) resaveModels(args.toSeq)
    else                 resaveAllModels()
  }

  def resaveModels(paths: Seq[String]): Unit = {
    val (systemDynamicsModels, otherModels) =
      paths.map((s: String) => Paths.get(s)).partition(_.toString.contains("System Dynamics"))

    if (systemDynamicsModels.isEmpty)
      System.setProperty("java.awt.headless", "true")

    otherModels.foreach(p => resaveModel(p))

    if (systemDynamicsModels.nonEmpty)
      resaveSystemDynamicsModels(systemDynamicsModels)

    System.exit(0)
  }

  def resaveAllModels(): Unit = {
    traverseModels(Paths.get(modelsRoot), resaveModel _)

    val failedModels = resaveSystemDynamicsModels(systemDynamicsModels)

    println("FAILED MODELS:")
    println(failedModels.mkString("\n"))
  }

  lazy val literalParser =
    Femto.scalaSingleton[LiteralParser]("org.nlogo.parse.CompilerUtilities")

  def resaveModel(modelPath: Path): Unit = {
    if (modelPath.toString.contains("System Dynamics"))
      systemDynamicsModels = systemDynamicsModels :+ modelPath
    else {
      val ws = HeadlessWorkspace.newInstance
      val converter =
        fileformat.converter(ws.getExtensionManager, ws.getCompilationEnvironment,
          literalParser, fileformat.defaultAutoConvertables :+ SDMAutoConvertable) _
      val modelLoader =
        fileformat.standardLoader(ws.compiler.utilities)
          .addSerializer[Array[String], NLogoFormat](new NLogoSDMFormat())
          .addSerializer[NLogoXFormat.Section, NLogoXFormat](new NLogoXSDMFormat(ScalaXmlElementFactory))
      val controller = new ResaveController(modelPath.toUri)
      val dialect =
        if (modelPath.toString.toUpperCase.endsWith("3D")) NetLogoThreeDDialect
        else NetLogoLegacyDialect
      OpenModelFromURI(modelPath.toUri, controller, modelLoader, converter(dialect), Version).foreach { model =>
        SaveModel(model, modelLoader, controller, ws.modelTracker, Version).map(_.apply()) match {
          case Some(Success(u)) => println("resaved: " + u)
          case Some(Failure(e)) => println("errored resaving: " + modelPath.toString + " " + e.toString)
          case None => println("failed to resave: " + modelPath.toString)
        }
      }
    }
  }

  def traverseModels(modelRoot: Path, resave: Path => Unit): Unit = {
    Files.walkFileTree(modelRoot, new java.util.HashSet(), Int.MaxValue, new ResaveVisitor(resave))
  }

  class ResaveVisitor(resave: Path => Unit) extends FileVisitor[Path] {
    import java.nio.file.attribute.BasicFileAttributes

    val excludeFolders = Seq("TEST", "BIN", "PROJECT", "SRC")

    def postVisitDirectory(path: Path, error: java.io.IOException): FileVisitResult = {
      if (error != null) throw error
      FileVisitResult.CONTINUE
    }

    def preVisitDirectory(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val dirName = path.getFileName.toString
      if (excludeFolders.contains(dirName.toUpperCase))
        FileVisitResult.SKIP_SUBTREE
      else
        FileVisitResult.CONTINUE
    }

    def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val fileName = path.getFileName.toString
      if (fileName.endsWith(".nlogo") || fileName.endsWith(".nlogo3d"))
        resave(path)
      FileVisitResult.CONTINUE
    }

    def visitFileFailed(path: Path, error: java.io.IOException): FileVisitResult = {
      throw error
      FileVisitResult.TERMINATE
    }
  }

  def resaveSystemDynamicsModels(paths: Seq[Path]): Seq[(Path, String)] = {
    App.main(Array[String]())

    var failedModels = List[(Path, String)]()

    for (path <- paths) {
      wait {
        try {
          val controller = new ResaveController(path.toUri)
          App.app.open(path.toString)
          App.app.saveOpenModel(controller)
        }
        catch {
          case e: Exception => failedModels :+= ((path, e.getMessage))
        }
      }
    }
    wait {
      App.app.quit()
    }
    failedModels
  }

  class ResaveController(path: URI) extends OpenModelController with SaveModelController {
    // SaveModelController
    def chooseFilePath(modelType: org.nlogo.api.ModelType): Option[java.net.URI] = {
      val pathSegments = path.toString.split("\\.")
      val nlogoXPath = pathSegments.init
      Some(new URI((nlogoXPath :+ "nlogox").mkString(".")))
    }
    def shouldSaveModelOfDifferingVersion(version: String): Boolean = true
    def warnInvalidFileFormat(format: String): Unit = {
      println("asked to save model in invalid format \"" + format + "\"")
    }

    // OpenModelController
    def errorOpeningURI(uri: java.net.URI,exception: Exception): Unit = {
      println("error opening model at: " + uri.toString + " - " + exception.toString)
    }
    def invalidModel(uri: java.net.URI): Unit = {
      println("invalid NetLogo model at: " + uri.toString)
    }
    def invalidModelVersion(uri: java.net.URI,version: String): Unit = {
      println("invalid Model version: \"" + version + "\" at: " + uri.toString)
    }
    def errorAutoconvertingModel(res: FailedConversionResult): Option[Model] = {
      println("Autoconversion failed for model at: " + path)
      println("errors:")
      res.errors.foreach(_.errors.foreach { e =>
        println(e.getMessage)
        e.printStackTrace()
      })
      None
    }
    def shouldOpenModelOfDifferingArity(arity: Int,version: String): Boolean = false
    def shouldOpenModelOfLegacyVersion(version: String): Boolean = true
    def shouldOpenModelOfUnknownVersion(version: String): Boolean = false
  }
}
