package org.enso.launcher.releases.fallback.staticwebsite

import io.circe.Decoder

import scala.util.Try

/** Describes a release available in a [[FileStorageFallbackReleaseProvider]].
  *
  * That release description can be used to create a concrete
  * [[FallbackRelease]] by adding the release provider specific information.
  */
case class ReleaseDescription(tag: String, assetNames: Seq[String])

/** Contains the list of releases available in a
  * [[FileStorageFallbackReleaseProvider]].
  */
case class ReleaseList(releases: Seq[ReleaseDescription])

object ReleaseList {

  /** Defines a part of the URL scheme of the fallback mechanism - the name of
    * the file describing the contents of all available releases.
    *
    * That must *never* be changed to ensure that all older launcher versions
    * can be upgraded.
    */
  val fileName = "release-list.json"

  private object Fields {
    val releases = "releases"
    val tag      = "tag"
    val assets   = "assets"
  }

  /** [[Decoder]] instance for [[ReleaseDescription]].
    *
    * It should always remain backwards compatible, since the fallback mechanism
    * must work for all released launcher versions.
    */
  implicit val releaseDecoder: Decoder[ReleaseDescription] = { json =>
    for {
      tag    <- json.get[String](Fields.tag)
      assets <- json.get[Seq[String]](Fields.assets)
    } yield ReleaseDescription(tag, assets)
  }

  /** [[Decoder]] instance for [[ReleaseList]].
    *
    * It should always remain backwards compatible, since the fallback mechanism
    * must work for all released launcher versions.
    */
  implicit val decoder: Decoder[ReleaseList] = { json =>
    for {
      releases <- json.get[Seq[ReleaseDescription]](Fields.releases)
    } yield ReleaseList(releases)
  }

  def parseString(string: String): Try[ReleaseList] =
    io.circe.parser.parse(string).flatMap(_.as[ReleaseList]).toTry
}
