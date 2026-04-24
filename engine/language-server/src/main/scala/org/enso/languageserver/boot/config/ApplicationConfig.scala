package org.enso.languageserver.boot.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._

/** An `application.conf` configuration. */
case class ApplicationConfig(ydoc: YdocConfig)

object ApplicationConfig {

  private val ConfigFilename  = "application-ls.conf"
  private val ConfigNamespace = "language-server"

  /** Load the configuration from the config file. */
  def load(): ApplicationConfig = {
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

}
