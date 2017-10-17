// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app

import org.nlogo.api.{ ModelLoader, ModelSections, TwoDVersion, ThreeDVersion, Version }
import org.nlogo.core.Model

class ModelSaver(model: ModelSections, loader: ModelLoader) {

  private var _currentModel: Model = Model()

  def priorModel: Model = _currentModel

  def currentModel = {
    val m = _currentModel.copy(
      code         = model.procedureSource,
      widgets      = model.widgets,
      info         = model.info,
      turtleShapes = model.turtleShapes,
      linkShapes   = model.linkShapes,
      version      = model.version)
    if (model.additionalSections.isEmpty)
      m
    else
      model.additionalSections.foldLeft(m) {
        case (newModel, section) => section.updateModel(newModel)
      }
  }

  def currentVersion: Version =
    if (Version.is3D(model.version)) ThreeDVersion
    else TwoDVersion

  def currentModelInCurrentVersion: Model = currentModel

  def setCurrentModel(m: Model) = {
    _currentModel = m
  }

  // this is only used by Modeling Commons and NetLogo Web Export
  // at the moment. It should *not* be used by anything else
  // (and it shouldn't be used by NLW or MC either if they can be changed).
  private[nlogo] def modelAsString(model: Model, format: String): String = {
    loader.sourceString(model, format).get
  }
}
