package org.enso.launcher.releases.github

import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Redirect
import java.nio.file.Path

import io.circe._
import io.circe.parser._
import org.enso.launcher.Logger

object GithubAPI {
  type ErrorT[T] = Either[Exception, T]
  case class Repository(owner: String, name: String)
  case class Release(assets: Seq[Asset])
  case class Asset(name: String, url: String)

  def listReleases(repo: Repository): ErrorT[Seq[String]] = {
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
  def getRelease(repo: Repository, tag: String): ErrorT[Release] = {
    val uri     = projectURI(repo) / "releases/" / "tags/" / encode(tag)
    val request = HttpRequest.newBuilder(uri).GET().build()
    println(request.uri())
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    Logger.debug(response.body())
    parse(response.body()).flatMap(_.as[Release])
  }

  def fetchTextAsset(asset: Asset): ErrorT[String]          = ???
  def downloadAsset(asset: Asset, path: Path): ErrorT[Unit] = ???

  private val baseUrl: URI = URI.create("https://api.github.com/")
  private def projectURI(project: Repository): URI = {
    val owner = s"${encode(project.owner)}/"
    val name  = s"${encode(project.name)}/"
    baseUrl / "repos/" / owner / name
  }

  private lazy val client =
    HttpClient.newBuilder().followRedirects(Redirect.NORMAL).build()

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
    } yield Asset(url, name)
  }

  implicit val releaseDecoder: Decoder[Release] = { json =>
    for {
      assets <- json.get[Seq[Asset]]("assets")
    } yield Release(assets)
  }
}
