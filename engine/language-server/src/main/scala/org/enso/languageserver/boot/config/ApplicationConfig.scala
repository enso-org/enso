package org.enso.languageserver.boot.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._

/** An `application.conf` configuration. */
case class ApplicationConfig(ydoc: YdocConfig)

object ApplicationConfig {

  private val ConfigFilename  = "application-ls.conf"
  private val ConfigNamespace = "language-server"

  /** Load the configuration from the config file. */
  private def loadConfig(): ApplicationConfig = {
    val contextClassLoader = Thread.currentThread().getContextClassLoader
    try {
      Thread.currentThread().setContextClassLoader(getClass.getClassLoader)
      ConfigSource
        .resources(ConfigFilename)
        .withFallback(ConfigSource.systemProperties)
        .at(ConfigNamespace)
        .loadOrThrow[ApplicationConfig]
    } finally Thread.currentThread().setContextClassLoader(contextClassLoader)
  }

  /** Override the configuration with environment variables.
    *
    * This is a workaround for the issue that the standard config overloading
    * does not work in the native image.
    */
  private def overrideConfig(config: ApplicationConfig): ApplicationConfig = {
    val ydocHostname =
      sys.env.getOrElse("LANGUAGE_SERVER_YDOC_HOSTNAME", config.ydoc.hostname)
    val ydocPort = sys.env
      .get("LANGUAGE_SERVER_YDOC_PORT")
      .map(_.toInt)
      .getOrElse(config.ydoc.port)

    config.copy(ydoc = YdocConfig(hostname = ydocHostname, port = ydocPort))
  }

  def load(): ApplicationConfig = {
    overrideConfig(loadConfig())
  }

}
