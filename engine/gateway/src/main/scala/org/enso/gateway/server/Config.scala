package org.enso.gateway.server

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

/** Describes endpoint to which [[org.enso.gateway.Server]] can bind and
  * timeouts.
  *
  * Gets default values of parameters from typesafe config.
  *
  * @param port           Port of endpoint.
  * @param host           Host of endpoint.
  * @param route          Route of endpoint.
  * @param timeout        Timeout for waiting response after request.
  * @param bindingTimeout Timeout for waiting binding result.
  * @param hardDeadline   Timeout for waiting result of binding termination.
  */
class Config(
  val port: Int                      = Config.port,
  val host: String                   = Config.host,
  val route: String                  = Config.route,
  val timeout: FiniteDuration        = Config.timeout,
  val bindingTimeout: FiniteDuration = Config.bindingTimeout,
  val hardDeadline: FiniteDuration   = Config.hardDeadline
) {
  val addressString: String = s"ws://$host:$port"
}

object Config {
  private val gatewayPath        = "gateway"
  private val serverPath         = "server"
  private val hostPath           = "host"
  private val portPath           = "port"
  private val routePath          = "route"
  private val timeoutPath        = "timeoutSecs"
  private val bindingTimeoutPath = "bindingTimeoutSecs"
  private val hardDeadlinePath   = "hardDeadlineSecs"

  private val gatewayConfig: TypesafeConfig =
    ConfigFactory.load.getConfig(gatewayPath)
  private val serverConfig: TypesafeConfig =
    gatewayConfig.getConfig(serverPath)

  private val host: String  = serverConfig.getString(hostPath)
  private val port: Int     = serverConfig.getInt(portPath)
  private val route: String = serverConfig.getString(routePath)
  private val timeout: FiniteDuration =
    serverConfig.getLong(timeoutPath).seconds
  private val bindingTimeout: FiniteDuration =
    serverConfig.getLong(bindingTimeoutPath).seconds
  private val hardDeadline: FiniteDuration =
    serverConfig.getLong(hardDeadlinePath).seconds
}
