package org.enso.launcher.http

import java.io.FileOutputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import org.apache.commons.io.IOUtils
import org.apache.http.client.config.{CookieSpecs, RequestConfig}
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.impl.client.HttpClients
import org.apache.http.{Header, HttpResponse}
import org.enso.cli.{TaskProgress, TaskProgressImplementation}
import org.enso.launcher.Logger
import org.enso.launcher.internal.{ProgressInputStream, ReadProgress}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Using}

/**
  * Contains the response contents as a string alongside with the headers
  * included in the response.
  *
  * @param content the response decoded as a string
  * @param headers sequence of headers included in the response
  */
case class APIResponse(content: String, headers: Seq[Header])

/**
  * Contains utility functions for fetching data using the HTTP(S) protocol.
  */
object HTTPDownload {

  /**
    * Fetches the `request` and tries to decode is as a [[String]].
    *
    * The request is executed in a separate thread. A [[TaskProgress]] instance
    * is returned immediately which can be used to track progress of the
    * download. The result contains the decoded response and included headers.
    *
    * @param request the request to send
    * @param sizeHint an optional hint indicating the expected size of the
    *                 response. It is used if the response does not include
    *                 explicit Content-Length header.
    * @param encoding the encoding used to decode the response content into a
    *                 string. By default, UTF-8 is used.
    * @return a [[TaskProgress]] that tracks progress of the download and can
    *         be used to get the final result
    */
  def fetchString(
    request: HttpUriRequest,
    sizeHint: Option[Long] = None,
    encoding: Charset      = StandardCharsets.UTF_8
  ): TaskProgress[APIResponse] = {
    Logger.debug(s"Fetching ${request.getURI.toASCIIString}")
    runRequest(request, asStringResponseHandler(sizeHint, encoding))
  }

  /**
    * Downloads the `request` and saves the response in the file pointed by the
    * `destination`.
    *
    * The request is executed in a separate thread. A [[TaskProgress]] instance
    * is returned immediately which can be used to track progress of the
    * download. The result is the same path as `destination`. It is available
    * only when the download has been completed successfully.
    *
    * @param request the request to send
    * @param sizeHint an optional hint indicating the expected size of the
    *                 response. It is used if the response does not include
    *                 explicit Content-Length header.
    * @return a [[TaskProgress]] that tracks progress of the download and can
    *         be used to wait for the completion of the download.
    */
  def download(
    request: HttpUriRequest,
    destination: Path,
    sizeHint: Option[Long] = None
  ): TaskProgress[Path] = {
    Logger.debug(s"Downloading ${request.getURI.toASCIIString} to $destination")
    runRequest(request, asFileHandler(destination, sizeHint))
  }

  /**
    * Creates a new thread that will send the provided request and returns a
    * [[TaskProgress]] monitoring progress of that request.
    *
    * @param request the request to send
    * @param handler a handler that processes the the received response to
    *                produce some result. The second argument of the handler is
    *                a callback that should be used by the handler to report
    *                progress.
    * @tparam A type of the result generated on success
    * @return [[TaskProgress]] that monitors request progress and will return
    *        the response returned by the `handler`
    */
  private def runRequest[A](
    request: HttpUriRequest,
    handler: (HttpResponse, ReadProgress => Unit) => A
  ): TaskProgress[A] = {
    val task = new TaskProgressImplementation[A]
    def update(progress: ReadProgress): Unit = {
      task.reportProgress(progress.alreadyRead(), progress.total())
    }

    def run(): Unit = {
      try {
        val client = buildClient()
        Using(client.execute(request)) { response =>
          val result = handler(response, update)
          task.setComplete(Success(result))
        }.get
      } catch {
        case NonFatal(e) => task.setComplete(Failure(e))
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

  /**
    * Creates a handler that tries to decode the result content as a [[String]].
    */
  private def asStringResponseHandler(
    sizeHint: Option[Long],
    charset: Charset
  )(response: HttpResponse, update: ReadProgress => Unit): APIResponse =
    Using(streamOfResponse(response, update, sizeHint)) { in =>
      val bytes   = in.readAllBytes()
      val content = new String(bytes, charset)
      APIResponse(content, response.getAllHeaders.toIndexedSeq)
    }.get

  /**
    * Creates a handler that tries to save the result content into a file.
    */
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

  /**
    * Returns a progress-monitored stream that can be used to read the response
    * content.
    */
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
}
