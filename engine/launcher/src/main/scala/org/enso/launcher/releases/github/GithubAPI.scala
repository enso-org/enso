package org.enso.launcher.releases.github

import java.net.URI
import java.net.http.HttpRequest
import java.nio.file.Path

import io.circe._
import io.circe.parser._
import org.enso.cli.TaskProgress

import scala.util.{Success, Try}
import org.enso.launcher.Logger

object GithubAPI {
  case class Repository(owner: String, name: String)
  case class Release(tag: String, assets: Seq[Asset])
  case class Asset(name: String, url: String, size: Long)

  def listReleases(repo: Repository): Try[Seq[Release]] = {
    val perPage = 100
    def listPage(page: Int): Try[Seq[Release]] = {
      val uri = (projectURI(repo) / "releases") ?
        ("per_page" -> perPage.toString) ? ("page" -> page.toString)
      val request =
        HttpRequest
          .newBuilder(uri)
          .GET()
          .build()
      Logger.debug(uri.toASCIIString)
      HTTPDownload
        .fetchString(request)
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
    val uri = projectURI(repo) / "releases/" / "tags/" / tag
    Logger.debug(uri.toString)
    val request =
      HttpRequest
        .newBuilder(uri)
        .GET()
        .build()
    HTTPDownload
      .fetchString(request)
      .flatMap(content =>
        parse(content)
          .flatMap(_.as[Release])
          .left
          .map(err => new RuntimeException(s"Cannot find release `$tag`.", err))
          .toTry
      )
  }

  def fetchTextAsset(asset: Asset): TaskProgress[String] = {
    val request =
      HttpRequest
        .newBuilder(URI.create(asset.url))
        .GET()
        .setHeader("Accept", "application/octet-stream")
        .build()
    HTTPDownload.fetchString(request, Some(asset.size))
  }

  def downloadAsset(asset: Asset, path: Path): TaskProgress[Unit] = {
    val request =
      HttpRequest
        .newBuilder(URI.create(asset.url))
        .GET()
        .setHeader("Accept", "application/octet-stream")
        .build()
    HTTPDownload.download(request, path, Some(asset.size)).map(_ => ())
  }

  private val baseUrl: URI = URI.create("https://api.github.com/")
  private def projectURI(project: Repository): URI = {
    val owner = s"${project.owner}/"
    val name  = s"${project.name}/"
    baseUrl / "repos/" / owner / name
  }

  implicit class URISyntax(uri: URI) {
    def /(part: String): URI =
      uri.resolve(part)
    def ?(query: (String, String)): URI = {
      val part = s"${query._1}=${query._2}"
      val newQuery =
        if (uri.getQuery == null) part else uri.getQuery + "&" + part
      new URI(
        uri.getScheme,
        uri.getUserInfo,
        uri.getHost,
        uri.getPort,
        uri.getPath,
        newQuery,
        uri.getFragment
      )
    }
  }

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
