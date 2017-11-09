// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app

import org.nlogo.window.WorkspaceConfig
import org.nlogo.api.Pico

class AppConfig(val pico: Pico) {
  var workspaceConfig: WorkspaceConfig = WorkspaceConfig.default
  var menuBarFactory: StatefulMenuBarFactory = null
  var relaunch: Boolean = false
}
