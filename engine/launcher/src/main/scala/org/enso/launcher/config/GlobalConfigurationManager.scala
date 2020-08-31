package org.enso.launcher.config

import java.io.BufferedWriter
import java.nio.file.{Files, NoSuchFileException, Path}

import io.circe.{yaml, Json}
import io.circe.syntax._
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.Logger
import org.enso.launcher.components.ComponentsManager
import org.enso.launcher.installation.DistributionManager

import scala.util.{Failure, Success, Try, Using}

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

  def configLocation: Path =
    distributionManager.paths.config / GlobalConfigurationManager.globalConfigName

  def getConfig: GlobalConfig =
    GlobalConfigurationManager
      .readConfig(configLocation)
      .recoverWith {
        case _: NoSuchFileException =>
          Logger.debug(
            s"Global config (at ${configLocation.toAbsolutePath} not found, " +
            s"falling back to defaults."
          )
          Success(GlobalConfig.Default)
      }
      .get

  def updateConfig(update: GlobalConfig => GlobalConfig): Unit = {
    val updated = update(getConfig)
    GlobalConfigurationManager.writeConfig(configLocation, updated).get
  }

  def updateConfigRaw(key: String, value: Json): Unit = {
    val updated = GlobalConfig.encoder(getConfig).asObject.get.add(key, value)
    GlobalConfigurationManager
      .writeConfigRaw(configLocation, updated.asJson)
      .recoverWith {
        case e: InvalidConfigError =>
          Failure(
            InvalidConfigError(
              s"Invalid value for key `$key`. Config changes were not saved.",
              e
            )
          )
      }
      .get
  }

  def removeFromConfig(key: String): Unit = {
    val updated = GlobalConfig.encoder(getConfig).asObject.get.remove(key)
    GlobalConfigurationManager.writeConfigRaw(configLocation, updated.asJson)
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
    writeConfigRaw(path, GlobalConfig.encoder(config))

  def writeConfigRaw(path: Path, rawConfig: Json): Try[Unit] = {
    def verifyConfig: Try[Unit] =
      rawConfig.as[GlobalConfig] match {
        case Left(failure) =>
          Failure(
            InvalidConfigError(
              s"Cannot parse modified config. Config changes were not saved.",
              failure
            )
          )
        case Right(_) => Success(())
      }
    def bufferedWriter: BufferedWriter = {
      Files.createDirectories(path.getParent)
      Files.newBufferedWriter(path)
    }
    def writeConfig =
      Using(bufferedWriter) { writer =>
        val string = yaml.Printer.spaces2
          .copy(preserveOrder = true)
          .pretty(rawConfig)
        writer.write(string)
        writer.newLine()
      }

    for {
      _ <- verifyConfig
      _ <- writeConfig
    } yield ()
  }
}
