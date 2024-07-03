package org.enso.editions

import org.enso.editions.Editions.Raw
import org.enso.yaml.YamlHelper

import java.nio.file.Path
import scala.util.{Failure, Try}

/** Gathers methods for decoding and encoding of Raw editions. */
object EditionSerialization {

  /** Tries to parse an edition definition from a string in the YAML format. */
  def parseYamlString(yamlString: String): Try[Raw.Edition] =
    YamlHelper
      .parseString[Raw.Edition](yamlString)
      .left
      .map(EditionResolutionError.EditionParseError)
      .toTry

  /** Tries to load an edition definition from a YAML file. */
  def loadEdition(path: Path): Try[Raw.Edition] =
    YamlHelper
      .load[Raw.Edition](path)
      .recoverWith { error =>
        Failure(
          EditionResolutionError.wrapLoadingError(
            path.getFileName.toString,
            error
          )
        )
      }

  object Fields {
    val name                = "name"
    val version             = "version"
    val repository          = "repository"
    val url                 = "url"
    val parent              = "extends"
    val engineVersion       = "engine-version"
    val repositories        = "repositories"
    val libraries           = "libraries"
    val localRepositoryName = "local"
  }

  case class EditionLoadingError(message: String, cause: Throwable)
      extends RuntimeException(message, cause) {

    /** @inheritdoc */
    override def toString: String = message
  }

}
