package org.enso.launcher.config

import java.nio.file.{Files, NoSuchFileException, Path}

import io.circe.yaml
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.Logger
import org.enso.launcher.components.ComponentsManager
import org.enso.launcher.installation.DistributionManager

import scala.util.{Success, Try, Using}

/**
  * Manages the global configuration of the distribution which includes the
  * default engine version and default project metadata used for new projects.
  */
class GlobalConfigurationManager(
  componentsManager: ComponentsManager,
  distributionManager: DistributionManager
) {

  /**
    * Returns the default Enso version that should be used when running Enso
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
            .map(_.version)
            .sorted
            .lastOption
        latestInstalled.getOrElse {
          val latestAvailable = componentsManager.fetchLatestEngineVersion()
          Logger.warn(
            s"No Enso versions installed, defaulting to the latest available " +
            s"release: $latestAvailable."
          )
          latestAvailable
        }
    }

  private def configLocation: Path =
    distributionManager.paths.config / GlobalConfigurationManager.globalConfigName

  def getConfig: GlobalConfig =
    GlobalConfigurationManager
      .readConfig(configLocation)
      .recoverWith {
        case _: NoSuchFileException =>
          Success(GlobalConfig.Default)
      }
      .get

  def updateConfig(update: GlobalConfig => GlobalConfig): Unit = {
    val updated = update(getConfig)
    GlobalConfigurationManager.writeConfig(configLocation, updated)
  }
}

object GlobalConfigurationManager {

  /**
    * Name of the main global configuration file.
    */
  val globalConfigName: String = "global-config.yaml"

  def readConfig(path: Path): Try[GlobalConfig] =
    Using(Files.newBufferedReader(path)) { reader =>
      for {
        json   <- yaml.parser.parse(reader)
        config <- json.as[GlobalConfig]
      } yield config
    }.flatMap(_.toTry)

  def writeConfig(path: Path, config: GlobalConfig): Try[Unit] =
    Using(Files.newBufferedWriter(path)) { writer =>
      val string = yaml.Printer.spaces2
        .copy(preserveOrder = true)
        .pretty(GlobalConfig.encoder(config))
      writer.write(string)
      writer.newLine()
    }
}
