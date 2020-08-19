package org.enso.launcher

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.components.ComponentsManager

/**
  * Manages the global configuration of the distribution which includes the
  * default engine version and default project metadata used for new projects.
  *
  * TODO [RW] This is a stub. It will be implemented in #977
  */
class GlobalConfigurationManager(componentsManager: ComponentsManager) {

  /**
    * Returns the default Enso version that should be used when running Enso
    * outside a project and when creating a new project.
    *
    * The default can be set by `enso default <version>` (TODO [RW] #977). If
    * the default is not set, the latest installed version is used. If no
    * versions are installed, the release provider is queried for the latest
    * available version.
    */
  def defaultVersion: SemVer = {
    val latestInstalled =
      componentsManager.listInstalledEngines().map(_.version).sorted.lastOption
    latestInstalled.getOrElse {
      val latestAvailable = componentsManager.fetchLatestEngineVersion()
      Logger.warn(
        s"No Enso versions installed, defaulting to the latest available " +
        s"release: $latestAvailable."
      )
      latestAvailable
    }
  }
}

object GlobalConfigurationManager {

  /**
    * Name of the main global configuration file.
    */
  val globalConfigName: String = "global-config.yml"
}
