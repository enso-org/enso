package org.enso.projectmanager.service.config

import io.circe.Json
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.service.config.GlobalConfigServiceFailure.ConfigurationFileAccessFailure
import org.enso.projectmanager.service.versionmanagement.NoOpInterface
import org.enso.projectmanager.versionmanagement.DistributionManagementConfiguration
import org.enso.runtimeversionmanager.config.GlobalConfigurationManager

/** Implementation of global configuration management logic.
  *
  * @param managers a distribution configuration
  */
class GlobalConfigService[F[+_, +_]: Sync: ErrorChannel](
  managers: DistributionManagementConfiguration
) extends GlobalConfigServiceApi[F] {

  val configurationManager = new GlobalConfigurationManager(
    managers.makeRuntimeVersionManager(new NoOpInterface),
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
