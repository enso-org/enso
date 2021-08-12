package org.enso.runtimeversionmanager.config

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.distribution.DistributionManager
import org.enso.distribution.config.{DefaultVersion, GlobalConfigurationManager}
import org.enso.runtimeversionmanager.components.RuntimeVersionManager

/** Manages the global configuration of the distribution and is able to infer
  * the default engine version.
  */
class GlobalRunnerConfigurationManager(
  componentsManager: RuntimeVersionManager,
  distributionManager: DistributionManager
) extends GlobalConfigurationManager(distributionManager) {

  private val logger = Logger[this.type]

  /** Returns the default Enso version that should be used when running Enso
    * outside a project and when creating a new project.
    *
    * The default can be set by `enso default <version>`. If the default is not
    * set, the latest installed version is used. If no versions are installed,
    * the release provider is queried for the latest available version.
    */
  def defaultVersion: SemVer =
    getConfig.defaultVersion match {
      case DefaultVersion.Exact(version) => version
      case DefaultVersion.LatestInstalled =>
        val latestInstalled =
          componentsManager
            .listInstalledEngines()
            .filter(!_.isMarkedBroken)
            .map(_.version)
            .sorted
            .lastOption
        latestInstalled.getOrElse {
          val latestAvailable = componentsManager.fetchLatestEngineVersion()
          logger.warn(
            "No Enso versions installed, defaulting to the latest available " +
            "release: [{}].",
            latestAvailable
          )
          latestAvailable
        }
    }
}
