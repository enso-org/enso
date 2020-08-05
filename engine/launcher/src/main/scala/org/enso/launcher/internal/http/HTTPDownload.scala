package org.enso.launcher.internal.http

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import org.apache.commons.io.IOUtils
import org.apache.http.HttpResponse
import org.apache.http.client.config.{CookieSpecs, RequestConfig}
import org.apache.http.client.methods.{HttpGet, HttpUriRequest}
import org.apache.http.impl.client.HttpClients
import org.enso.cli.{TaskProgress, TaskProgressImplementation}
import org.enso.launcher.Logger
import org.enso.launcher.internal.{ProgressInputStream, ReadProgress}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Using}

object HTTPDownload {
  def fetchString(
    request: HttpUriRequest,
    sizeHint: Option[Long] = None
  ): TaskProgress[String] = {
    Logger.debug(s"Fetching ${request.getURI.toASCIIString}")
    runRequest(request, asStringHandler(sizeHint))
  }

  def download(
    request: HttpUriRequest,
    destination: Path,
    sizeHint: Option[Long] = None
  ): TaskProgress[Path] = {
    Logger.debug(s"Downloading ${request.getURI.toASCIIString} to $destination")
    runRequest(request, asFileHandler(destination, sizeHint))
  }

  private def runRequest[A](
    request: HttpUriRequest,
    handler: (HttpResponse, ReadProgress => Unit) => A
  ): TaskProgress[A] = {
    val task = new TaskProgressImplementation[A]
    def update(progress: ReadProgress): Unit = {
      task.reportProgress(progress.alreadyRead(), progress.total())
    }

    def run(): Unit = {
      val client = buildClient()
      Using(client.execute(request)) { response =>
        try {
          val result = handler(response, update)
          task.setComplete(Success(result))
        } catch {
          case NonFatal(e) => task.setComplete(Failure(e))
        }
      }
    }

    val thread = new Thread(() => run(), "HTTP-Runner")
    thread.start()
    task
  }

  private def buildClient() =
    HttpClients
      .custom()
      .setDefaultRequestConfig(
        RequestConfig.custom().setCookieSpec(CookieSpecs.STANDARD).build()
      )
      .build()

  private def asStringHandler(
    sizeHint: Option[Long]
  )(response: HttpResponse, update: ReadProgress => Unit): String =
    Using(streamOfResponse(response, update, sizeHint)) { in =>
      val bytes = in.readAllBytes()
      new String(bytes, StandardCharsets.UTF_8)
    }.get

  private def asFileHandler(
    path: Path,
    sizeHint: Option[Long]
  )(response: HttpResponse, update: ReadProgress => Unit): Path =
    Using(streamOfResponse(response, update, sizeHint)) { in =>
      Using(new FileOutputStream(path.toFile)) { out =>
        IOUtils.copy(in, out)
        path
      }.get
    }.get

  private def streamOfResponse(
    response: HttpResponse,
    update: ReadProgress => Unit,
    sizeHint: Option[Long]
  ): ProgressInputStream = {
    val entity = response.getEntity
    val size = {
      val len = entity.getContentLength
      if (len < 0) None
      else Some(len)
    }

    new ProgressInputStream(entity.getContent, size.orElse(sizeHint), update)
  }

  def simulate(): Unit = {
    val get = new HttpGet("https://example.com/")
    println(fetchString(get).waitForResult(true))

    val get2 = new HttpGet(
      "https://github.com/enso-org/enso-staging/releases/download/enso-0.1.0/enso-engine-0.1.0.zip"
    )
    println(download(get2, Path.of("engine.zip")).waitForResult(true))
  }

}
