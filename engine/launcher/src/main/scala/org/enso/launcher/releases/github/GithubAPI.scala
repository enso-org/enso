package org.enso.launcher.releases.github

import java.nio.file.Path

import io.circe._
import io.circe.parser._
import org.apache.http.Header
import org.enso.cli.TaskProgress
import org.enso.launcher.internal.http.{
  APIResponse,
  HTTPDownload,
  HTTPRequestBuilder,
  URIBuilder
}
import org.enso.launcher.releases.ReleaseProviderException

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
        .flatMap(response =>
          parse(response.content)
            .flatMap(
              _.as[Seq[Release]].left
                .map(err =>
                  handleError(
                    response,
                    ReleaseProviderException(s"Cannot fetch release list.", err)
                  )
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
      .flatMap(response =>
        parse(response.content)
          .flatMap(_.as[Release])
          .left
          .map(err =>
            handleError(
              response,
              ReleaseProviderException(s"Cannot find release `$tag`.", err)
            )
          )
          .toTry
      )
  }

  private def handleError(
    response: APIResponse,
    defaultError: => Throwable
  ): Throwable = {
    def isLimitExceeded(header: Header): Boolean =
      header.getValue.toIntOption.contains(0)

    response.headers.find(_.getName == "X-RateLimit-Remaining") match {
      case Some(header) if isLimitExceeded(header) =>
        ReleaseProviderException(
          "GitHub Release API rate limit exceeded for your IP address. " +
          "Please try again in a while."
        )
      case _ => defaultError
    }
  }

  def fetchTextAsset(asset: Asset): TaskProgress[String] = {
    val request = HTTPRequestBuilder
      .fromURL(asset.url)
      .setHeader("Accept", "application/octet-stream")
      .GET

    HTTPDownload.fetchString(request, Some(asset.size)).map(_.content)
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
      url  <- json.get[String]("browser_download_url")
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
