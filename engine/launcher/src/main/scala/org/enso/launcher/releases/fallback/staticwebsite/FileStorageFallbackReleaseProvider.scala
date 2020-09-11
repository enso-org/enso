package org.enso.launcher.releases.fallback.staticwebsite

import org.enso.launcher.releases.fallback.FallbackReleaseProvider
import org.enso.launcher.releases.{Release, ReleaseProviderException}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Implements a fallback release provider which queries a simple file
  * [[storage]].
  *
  * It has the following requirements:
  * 1. It should contain a file `fallback-manifest.yaml` with a key `enabled` in
  *    the root directory. This key determines if the provider [[isAvailable]].
  * 2. It should contain a file `release-list.json` in the root directory.
  *    That file should contain a key `releases` which should contain a list of
  *    keys whose names are tags of the releases and their values are lists of
  *    their assets.
  * 3. A directory [[releasesDirectory]] that contains subdirectories for each
  *    release tag which contain the listed assets.
  */
class FileStorageFallbackReleaseProvider(
  storage: FileStorage,
  releasesDirectory: String
) extends FallbackReleaseProvider {

  /**
    * @inheritdoc
    */
  override def isAvailable: Boolean = isAvailableCached

  /**
    * @inheritdoc
    */
  override def releaseForTag(tag: String): Try[Release] =
    if (!isAvailable) {
      Failure(new IllegalStateException("The provider is unavailable."))
    } else
      cachedReleaseList.flatMap { releases =>
        releases
          .find(_.tag == tag)
          .map(Success(_))
          .getOrElse(
            Failure(
              ReleaseProviderException(
                s"Release for tag `$tag` is not available."
              )
            )
          )
      }

  /**
    * @inheritdoc
    */
  override def listReleases(): Try[Seq[Release]] =
    if (!isAvailable) {
      Failure(new IllegalStateException("The provider is unavailable."))
    } else cachedReleaseList

  private def fetchAsString(fileName: String): String =
    storage.fetchString(Seq(fileName)).waitForResult().get

  private lazy val isAvailableCached = queryIsAvailable()
  private def queryIsAvailable(): Boolean =
    try {
      val content  = fetchAsString(FallbackManifest.fileName)
      val manifest = FallbackManifest.parseString(content).get
      manifest.enabled
    } catch { case NonFatal(_) => false }

  private lazy val cachedReleaseList = fetchReleaseList()
  private def fetchReleaseList(): Try[Seq[Release]] =
    Try {
      val content     = fetchAsString(ReleaseList.fileName)
      val releaseList = ReleaseList.parseString(content).get
      releaseList.releases.map(
        FallbackRelease.fromDescription(
          _,
          storage      = storage,
          releasesRoot = Seq(releasesDirectory)
        )
      )
    }
}
