package org.enso.projectmanager.infrastructure.config

import java.io.File

import pureconfig.ConfigReader

/**
  * Custom ConfigReaders which allow for reading custom types
  * from configuration.
  */
object ConfigurationReaders {

  implicit val fileReader: ConfigReader[File] =
    ConfigReader[String].map(path => new File(path))

}
