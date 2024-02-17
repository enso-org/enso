package org.enso.distribution.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import com.github.zafarkhaja.semver.Version
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.editions.SemVerJson._

import scala.util.Try

/** Default version that is used when launching Enso outside of projects and
  * when creating new projects.
  */
sealed trait DefaultVersion
object DefaultVersion {

  /** Defaults to the latest installed version, or if no versions are installed,
    * to the latest available release.
    */
  case object LatestInstalled extends DefaultVersion {

    /** @inheritdoc
      */
    override def toString: String = "latest-installed"
  }

  /** Defaults to a specified version.
    */
  case class Exact(version: Version) extends DefaultVersion {

    /** @inheritdoc
      */
    override def toString: String = version.toString
  }

  /** [[Encoder]] instance for [[DefaultVersion]].
    */
  implicit val encoder: Encoder[DefaultVersion] = {
    case LatestInstalled =>
      Json.Null
    case Exact(version) =>
      version.asJson
  }

  /** [[Decoder]] instance for [[DefaultVersion]].
    */
  implicit val decoder: Decoder[DefaultVersion] = { json =>
    if (json.value.isNull) Right(LatestInstalled)
    else
      for {
        version <- json.as[Version]
      } yield Exact(version)
  }

  /** [[Argument]] instance for [[DefaultVersion]].
    */
  implicit val argument: Argument[DefaultVersion] = { string =>
    if (string == LatestInstalled.toString) Right(LatestInstalled)
    else {
      implicitly[Argument[Version]].read(string).map(Exact)
    }
  }

  /** [[Argument]] instance that tries to parse the String as a [[Version]]
    * version string.
    */
  implicit val semverArgument: Argument[Version] = (string: String) =>
    Try(Version.parse(string)).fold(
      _ =>
        Left(
          OptsParseError(s"`$string` is not a valid semantic version string.")
        ),
      v => Right(v)
    )
}
