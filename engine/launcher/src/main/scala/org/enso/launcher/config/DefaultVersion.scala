package org.enso.launcher.config

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.cli.Arguments._
import org.enso.pkg.SemVerJson._
import org.enso.cli.Argument

sealed trait DefaultVersion
object DefaultVersion {
  case object LatestInstalled extends DefaultVersion {
    override def toString: String = "latest-installed"
  }
  case class Exact(version: SemVer) extends DefaultVersion {
    override def toString: String = version.toString
  }

  implicit val encoder: Encoder[DefaultVersion] = {
    case LatestInstalled =>
      Json.Null
    case Exact(version) =>
      version.asJson
  }

  implicit val decoder: Decoder[DefaultVersion] = { json =>
    if (json.value.isNull) Right(LatestInstalled)
    else
      for {
        version <- json.as[SemVer]
      } yield Exact(version)
  }

  implicit val argument: Argument[DefaultVersion] = { string =>
    if (string == LatestInstalled.toString) Right(LatestInstalled)
    else {
      implicitly[Argument[SemVer]].read(string).map(Exact)
    }
  }
}
