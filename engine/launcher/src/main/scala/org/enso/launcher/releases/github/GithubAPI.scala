package org.enso.launcher.releases.github

import java.net.{URI, URLEncoder}
import java.nio.file.Path

import io.circe._
import io.circe.parser._
import org.enso.cli.ProgressBar
import org.enso.launcher.Logger
import org.enso.launcher.releases.PendingDownload

object GithubAPI {
  case class Repository(owner: String, name: String)
  case class Release(assets: Seq[Asset])
  case class Asset(name: String, url: String, size: Long)

  def listReleases(repo: Repository): PendingDownload[Seq[String]] = {
    /*val uri     = projectURI(repo) / "releases"
    val request = HttpRequest.newBuilder(uri).GET().build()
    println(request.uri())
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    case class Releases(tags: Seq[String])
    implicit val decoder: Decoder[Releases] = { json =>
      for {
        tags <- json.get[Seq[]]
      }
    }
    Logger.debug(response.body())
    parse(response.body()).flatMap(_.as[Release])*/
    // TODO
    ???
  }
  def getRelease(repo: Repository, tag: String): PendingDownload[Release] = {
    val uri = projectURI(repo) / "releases/" / "tags/" / encode(tag)
    Logger.debug(uri.toString)
    HTTPDownload
      .get(uri)
      .flatMap(content => parse(content).flatMap(_.as[Release]).toTry)
  }

  def simulate(): Unit = {
    val download = HTTPDownload.download(
      URI.create(
        "https://github.com/Luna-Tensorflow/Luna-Tensorflow/releases/download/betav1.3.1/tensorflow_luna_linux.tar.gz"
      ),
      Path.of("ltf.tar.gz")
    )
    val result = ProgressBar.waitWithProgress(download)
    println(result)
  }

  def fetchTextAsset(asset: Asset): PendingDownload[String] = {
    ???
  }
  def downloadAsset(asset: Asset, path: Path): PendingDownload[Unit] = ???

  private val baseUrl: URI = URI.create("https://api.github.com/")
  private def projectURI(project: Repository): URI = {
    val owner = s"${encode(project.owner)}/"
    val name  = s"${encode(project.name)}/"
    baseUrl / "repos/" / owner / name
  }

  private def encode(rawString: String): String =
    URLEncoder.encode(rawString, "UTF-8")

  implicit class URISyntax(uri: URI) {
    def /(part: String): URI =
      uri.resolve(part)
  }

  implicit val assetDecoder: Decoder[Asset] = { json =>
    for {
      url  <- json.get[String]("url")
      name <- json.get[String]("name")
      size <- json.get[Long]("size")
    } yield Asset(url, name, size)
  }

  implicit val releaseDecoder: Decoder[Release] = { json =>
    for {
      assets <- json.get[Seq[Asset]]("assets")
    } yield Release(assets)
  }
}
