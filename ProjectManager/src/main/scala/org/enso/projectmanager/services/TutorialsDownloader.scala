package org.enso.projectmanager.services

import java.io.File
import java.io.FileOutputStream
import java.time.Instant
import java.util.zip.ZipFile

import akka.actor.ActorSystem
import akka.event.LogSource
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.FileIO
import org.apache.commons.io.IOUtils
import spray.json.DefaultJsonProtocol
import spray.json.JsonParser

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

case class GithubTutorial(name: String, lastPushString: String) {
  val lastPush: Instant = Instant.parse(lastPushString)
}

trait GithubJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val tutorialFormat =
    jsonFormat(GithubTutorial, "name", "pushed_at")
}

case class HttpHelper()(
  implicit val executor: ExecutionContext,
  implicit val system: ActorSystem,
  implicit val materializer: ActorMaterializer) {

  def performWithRedirects(request: HttpRequest): Future[HttpResponse] =
    Http().singleRequest(request).flatMap(followRedirects)

  def followRedirects(response: HttpResponse): Future[HttpResponse] =
    response.status match {
      case StatusCodes.Found =>
        response.entity.discardBytes()
        val redirectUri = response.header[Location].get.uri
        val newRequest  = HttpRequest(uri = redirectUri)
        performWithRedirects(newRequest)
      case _ => Future(response)
    }
}

case class TutorialsDownloader(
  tutorialsDir: File,
  cacheDir: File,
  packagesGithubOrganisation: String
)(implicit val system: ActorSystem,
  implicit val executor: ExecutionContext,
  implicit val materializer: ActorMaterializer)
    extends GithubJsonProtocol {

  val packagesGithubUrl =
    s"https://api.github.com/orgs/$packagesGithubOrganisation"
  val packagesGithubReposUrl = s"$packagesGithubUrl/repos"

  implicit val logSource: LogSource[TutorialsDownloader] = _ =>
    "TutorialsDownloader"
  val logging = Logging(system, this)

  tutorialsDir.mkdirs()
  cacheDir.mkdirs()

  def downloadUrlFor(tutorial: GithubTutorial): String =
    s"https://github.com/$packagesGithubOrganisation/${tutorial.name}/archive/master.zip"

  def zipFileFor(tutorial: GithubTutorial): File =
    new File(cacheDir, s"${tutorial.name}.zip")

  def getAvailable: Future[List[GithubTutorial]] =
    Http()
      .singleRequest(HttpRequest(uri = packagesGithubReposUrl))
      .flatMap(resp => Unmarshal(resp.entity).to[String])
      .map(JsonParser(_).convertTo[List[GithubTutorial]])

  def downloadIfNeeded(tutorial: GithubTutorial): Future[File] =
    if (needsUpdate(tutorial)) {
      logging.info(s"Downloading zip archive for ${tutorial.name}")
      downloadZip(tutorial)
    }
    else {
      logging.info(s"Reusing cached zip archive for ${tutorial.name}")
      Future { zipFileFor(tutorial) }
    }

  def needsUpdate(tutorial: GithubTutorial): Boolean =
    zipFileFor(tutorial).lastModified < tutorial.lastPush.toEpochMilli

  def downloadZip(tutorial: GithubTutorial): Future[File] = {
    val downloadUrl = downloadUrlFor(tutorial)
    val request     = HttpRequest(uri = downloadUrl)
    val response    = HttpHelper().performWithRedirects(request)
    val target      = zipFileFor(tutorial)
    response
      .flatMap(_.entity.dataBytes.runWith(FileIO.toPath(target.toPath)))
      .map(_ => target)
  }

  def unzip(source: File): Unit = {
    logging.debug(s"Unzipping $source\n")
    val zipFileTry = Try(new ZipFile(source))
    val result = zipFileTry.map { zipFile =>
      zipFile.entries.asScala.foreach { entry =>
        logging.debug(s"Extracting ${entry.getName}\n")
        val target = new File(tutorialsDir, entry.getName)
        if (entry.isDirectory) target.mkdirs()
        else {
          target.getParentFile.mkdirs()
          val in  = zipFile.getInputStream(entry)
          val out = new FileOutputStream(target)
          IOUtils.copy(in, out)
          out.close()
        }
      }
    }
    zipFileTry.map(_.close())
    result.get
  }

  def run(): Future[Unit] = {
    val tutorialsFuture = getAvailable
    val zipsFuture = tutorialsFuture.flatMap { tutorials =>
      Future.sequence(
        tutorials.map(downloadIfNeeded)
      )
    }
    zipsFuture.map(_.foreach(unzip))
  }

}
