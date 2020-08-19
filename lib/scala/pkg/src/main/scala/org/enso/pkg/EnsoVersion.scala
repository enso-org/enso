package org.enso.pkg

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}
import nl.gn0s1s.bump.SemVer

/**
  * Represents the engine version that is associated with a project.
  */
sealed trait EnsoVersion

/**
  * Represents `default` Enso version.
  *
  * If `enso-version` is set to `default`, the locally default Enso engine
  * version is used for the project.
  */
case object DefaultEnsoVersion extends EnsoVersion {
  private val defaultEnsoVersion = "default"

  /**
    * @inheritdoc
    */
  override def toString: String = defaultEnsoVersion
}

/**
  * An exact semantic versioning string.
  */
case class SemVerEnsoVersion(version: SemVer) extends EnsoVersion {

  /**
    * @inheritdoc
    */
  override def toString: String = version.toString
}

object EnsoVersion {

  /**
    * [[Decoder]] instance allowing to parse [[EnsoVersion]].
    */
  implicit val decoder: Decoder[EnsoVersion] = { json =>
    json.as[String].flatMap { string =>
      if (string == DefaultEnsoVersion.toString)
        Right(DefaultEnsoVersion)
      else
        SemVer(string)
          .map(SemVerEnsoVersion)
          .toRight(
            DecodingFailure(
              s"`$string` is not a valid version string. Possible values are " +
              s"`default` or a semantic versioning string.",
              json.history
            )
          )
    }
  }

  /**
    * [[Encoder]] instance allowing to convert [[EnsoVersion]] to JSON or YAML.
    */
  implicit val encoder: Encoder[EnsoVersion] = { version =>
    version.toString.asJson
  }
}
