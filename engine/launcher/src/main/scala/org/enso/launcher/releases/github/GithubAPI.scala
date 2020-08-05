package org.enso.launcher.releases.github

import java.nio.file.Path

import io.circe._
import io.circe.parser._
import org.enso.cli.TaskProgress
import org.enso.launcher.internal.http.{
  HTTPDownload,
  HTTPRequestBuilder,
  URIBuilder
}

import scala.util.{Success, Try}

object GithubAPI {
  case class Repository(owner: String, name: String)
  case class Release(tag: String, assets: Seq[Asset])
  case class Asset(name: String, url: String, size: Long)

  def listReleases(repo: Repository): Try[Seq[Release]] = {
    val perPage = 100
    def listPage(page: Int): Try[Seq[Release]] = {
      val uri = (projectURI(repo) / "releases") ?
        ("per_page" -> perPage.toString) ? ("page" -> page.toString)

      HTTPDownload
        .fetchString(HTTPRequestBuilder.fromURI(uri.build()).GET)
        .flatMap(content =>
          parse(content)
            .flatMap(
              _.as[Seq[Release]].left
                .map(err =>
                  new RuntimeException(s"Cannot fetch release list.", err)
                )
            )
            .toTry
        )
        .waitForResult()
    }

    def listAllPages(from: Int): Try[Seq[Release]] =
      listPage(from).flatMap { current =>
        if (current.length == perPage)
          listAllPages(from + 1).map(current ++ _)
        else
          Success(current)
      }

    listAllPages(1)
  }

  def getRelease(repo: Repository, tag: String): TaskProgress[Release] = {
    val uri = projectURI(repo) / "releases" / "tags" / tag

    HTTPDownload
      .fetchString(HTTPRequestBuilder.fromURI(uri.build()).GET)
      .flatMap(content =>
        parse(content)
          .flatMap(_.as[Release])
          .left
          .map(err => new RuntimeException(s"Cannot find release `$tag`.", err))
          .toTry
      )
  }

  def fetchTextAsset(asset: Asset): TaskProgress[String] = {
    val request = HTTPRequestBuilder
      .fromURL(asset.url)
      .setHeader("Accept", "application/octet-stream")
      .GET

    HTTPDownload.fetchString(request, Some(asset.size))
  }

  def downloadAsset(asset: Asset, path: Path): TaskProgress[Unit] = {
    val request = HTTPRequestBuilder
      .fromURL(asset.url)
      .setHeader("Accept", "application/octet-stream")
      .GET

    HTTPDownload
      .download(request, path, Some(asset.size))
      .map(_ => ())
  }

  private val baseUrl =
    URIBuilder.fromHost("api.github.com")
  private def projectURI(project: Repository) =
    baseUrl / "repos" / project.owner / project.name

  implicit val assetDecoder: Decoder[Asset] = { json =>
    for {
      url  <- json.get[String]("url")
      name <- json.get[String]("name")
      size <- json.get[Long]("size")
    } yield Asset(name = name, url = url, size = size)
  }

  implicit val releaseDecoder: Decoder[Release] = { json =>
    for {
      tag    <- json.get[String]("tag_name")
      assets <- json.get[Seq[Asset]]("assets")
    } yield Release(tag, assets)
  }
}
