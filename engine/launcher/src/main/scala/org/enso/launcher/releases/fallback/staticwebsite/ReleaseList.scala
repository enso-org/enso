package org.enso.launcher.releases.fallback.staticwebsite

import io.circe.Decoder

import scala.util.Try

case class ReleaseDescription(tag: String, assetNames: Seq[String])
case class ReleaseList(releases: Seq[ReleaseDescription])

object ReleaseList {
  val fileName = "releases-list.yaml"

  private object Fields {
    val releases = "releases"
    val tag      = "tag"
    val assets   = "assets"
  }

  implicit val releaseDecoder: Decoder[ReleaseDescription] = { json =>
    for {
      tag    <- json.get[String](Fields.tag)
      assets <- json.get[Seq[String]](Fields.assets)
    } yield ReleaseDescription(tag, assets)
  }

  implicit val decoder: Decoder[ReleaseList] = { json =>
    for {
      releases <- json.get[Seq[ReleaseDescription]](Fields.releases)
    } yield ReleaseList(releases)
  }

  def parseYAML(string: String): Try[ReleaseList] =
    io.circe.yaml.parser.parse(string).flatMap(_.as[ReleaseList]).toTry
}
