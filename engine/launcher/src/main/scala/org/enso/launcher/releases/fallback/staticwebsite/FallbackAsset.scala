package org.enso.launcher.releases.fallback.staticwebsite

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.launcher.releases.Asset

case class FallbackAsset(fileName: String, releaseRoot: URIBuilder)
    extends Asset {

  /**
    * @inheritdoc
    */
  override def downloadTo(path: Path): TaskProgress[Unit] =
    HTTPDownload.download(request, destination = path).map(_ => ())

  /**
    * @inheritdoc
    */
  override def fetchAsText(): TaskProgress[String] =
    HTTPDownload.fetchString(request).map(_.content)

  private def request =
    HTTPRequestBuilder.fromURI((releaseRoot / fileName).build()).GET
}
