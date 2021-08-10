package org.enso.distribution.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import nl.gn0s1s.bump.SemVer
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.editions.SemVerJson._

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
  case class Exact(version: SemVer) extends DefaultVersion {

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
        version <- json.as[SemVer]
      } yield Exact(version)
  }

  /** [[Argument]] instance for [[DefaultVersion]].
    */
  implicit val argument: Argument[DefaultVersion] = { string =>
    if (string == LatestInstalled.toString) Right(LatestInstalled)
    else {
      implicitly[Argument[SemVer]].read(string).map(Exact)
    }
  }

  /** [[Argument]] instance that tries to parse the String as a [[SemVer]]
    * version string.
    */
  implicit val semverArgument: Argument[SemVer] = (string: String) =>
    SemVer(string).toRight(
      OptsParseError(s"`$string` is not a valid semantic version string.")
    )
}
