package org.enso.projectmanager.service.config

import io.circe.Json
import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.service.config.GlobalConfigServiceFailure.ConfigurationFileAccessFailure
import org.enso.projectmanager.versionmanagement.DistributionManagementConfiguration
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface
}
import org.enso.runtimeversionmanager.config.GlobalConfigurationManager
import org.enso.runtimeversionmanager.locking.Resource

class GlobalConfigService[F[+_, +_]: Sync: ErrorChannel](
  managers: DistributionManagementConfiguration
) extends GlobalConfigServiceApi[F] {

  class ConfigUserInterface extends RuntimeVersionManagementUserInterface {
    override def trackProgress(task: TaskProgress[_]): Unit           = ()
    override def shouldInstallMissingEngine(version: SemVer): Boolean = false
    override def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean =
      false
    override def shouldInstallBrokenEngine(version: SemVer): Boolean = false
    override def logInfo(message: => String): Unit                   = ()
    override def startWaitingForResource(resource: Resource): Unit   = ()
    override def finishWaitingForResource(resource: Resource): Unit  = ()
  }

  val configurationManager = new GlobalConfigurationManager(
    managers.makeRuntimeVersionManager(new ConfigUserInterface),
    managers.distributionManager
  )

  override def getKey(
    key: String
  ): F[GlobalConfigServiceFailure, Option[String]] =
    Sync[F].blockingIO {
      val valueOption = configurationManager.getConfig.original.apply(key)
      valueOption.map(json => json.asString.getOrElse(json.toString()))
    }.recoverAccessErrors

  override def setKey(
    key: String,
    value: String
  ): F[GlobalConfigServiceFailure, Unit] = Sync[F].blockingIO {
    configurationManager.updateConfigRaw(key, Json.fromString(value))
  }.recoverAccessErrors

  override def deleteKey(key: String): F[GlobalConfigServiceFailure, Unit] =
    Sync[F].blockingIO {
      configurationManager.removeFromConfig(key)
    }.recoverAccessErrors

  implicit class AccessErrorRecovery[A](fa: F[Throwable, A]) {
    def recoverAccessErrors: F[GlobalConfigServiceFailure, A] = {
      ErrorChannel[F].mapError(fa) { throwable =>
        ConfigurationFileAccessFailure(throwable.getMessage)
      }
    }
  }
}
