package org.enso.downloader.http

import com.typesafe.scalalogging.Logger
import org.enso.cli.task.{
  ProgressUnit,
  TaskProgress,
  TaskProgressImplementation
}

import java.net.URI
import java.net.http.{HttpClient, HttpHeaders, HttpResponse}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, Executors}
import scala.util.{Failure, Success}

/** Contains utility functions for fetching data using the HTTP(S) protocol. */
object HTTPDownload {
  private val logger = Logger[HTTPDownload.type]

  /** Determines how many redirects are taken until an error is thrown. */
  val maximumRedirects: Int = 20

  /** Fetches the `request` and tries to decode is as a [[String]].
    *
    * The request is executed in a separate thread. A [[TaskProgress]] instance
    * is returned immediately which can be used to track progress of the
    * download. The result contains the decoded response and included headers.
    *
    * Handles redirects, but will return an error if the amount of redirects
    * exceeds [[maximumRedirects]].
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
    request: HTTPRequest,
    sizeHint: Option[Long] = None,
    encoding: Charset      = StandardCharsets.UTF_8
  ): TaskProgress[APIResponse] = {
    logger.debug("Fetching [{}].", request.requestImpl.uri())
    val taskProgress =
      new TaskProgressImplementation[APIResponse](ProgressUnit.Bytes)
    val total: java.lang.Long = if (sizeHint.isDefined) sizeHint.get else null
    val bodyHandler =
      StringProgressBodyHandler.of(taskProgress, total, encoding)
    asyncDownload(request, bodyHandler)
      .thenAccept(res => {
        if (res.statusCode() == 404) {
          taskProgress.setComplete(Failure(ResourceNotFound()))
        } else {
          taskProgress.setComplete(
            Success(
              APIResponse(
                res.body(),
                convertHeaders(res.headers()),
                res.statusCode()
              )
            )
          )
        }
      })
      .exceptionally(ex => {
        taskProgress.setComplete(
          Failure(HTTPException(s"Server responded with: [${ex.getMessage}]."))
        )
        null
      })
    taskProgress
  }

  private def asyncDownload[T](
    request: HTTPRequest,
    bodyHandler: HttpResponse.BodyHandler[T]
  ): CompletableFuture[HttpResponse[T]] = {
    val vThreadExecutor = Executors.newVirtualThreadPerTaskExecutor()
    val httpClient = HttpClient
      .newBuilder()
      .followRedirects(HttpClient.Redirect.NORMAL)
      .executor(vThreadExecutor)
      .build()
    httpClient.sendAsync(request.requestImpl, bodyHandler)
  }

  private def convertHeaders(
    headers: HttpHeaders
  ): Seq[Header] = {
    val headerBuilder = Seq.newBuilder[Header]
    headers.map().forEach { case (name, values) =>
      values.forEach(value => {
        headerBuilder += Header(name, value)
      })
    }
    headerBuilder.result()
  }

  /** Fetches the `uri` and tries to decode is as a [[String]].
    *
    * It is a shorthand for the other variant of [[fetchString]] that creates a
    * simple GET request from the URI.
    *
    * @param uri the URI to query
    * @return a [[TaskProgress]] that tracks progress of the download and can
    *         be used to get the final result
    */
  def fetchString(uri: URI): TaskProgress[APIResponse] = {
    val request = HTTPRequestBuilder.fromURI(uri).GET
    fetchString(request)
  }

  /** Downloads the `request` and saves the response in the file pointed by the
    * `destination`.
    *
    * The request is executed in a separate thread. A [[TaskProgress]] instance
    * is returned immediately which can be used to track progress of the
    * download. The result is the same path as `destination`. It is available
    * only when the download has been completed successfully.
    *
    * Handles redirects, but will return an error if the amount of redirects
    * exceeds [[maximumRedirects]].
    *
    * @param request the request to send
    * @param sizeHint an optional hint indicating the expected size of the
    *                 response. It is used if the response does not include
    *                 explicit Content-Length header.
    * @return a [[TaskProgress]] that tracks progress of the download and can
    *         be used to wait for the completion of the download.
    */
  def download(
    request: HTTPRequest,
    destination: Path,
    sizeHint: Option[Long] = None
  ): TaskProgress[Path] = {
    logger.debug(
      "Downloading [{}] to [{}].",
      request.requestImpl.uri(),
      destination
    )
    val taskProgress          = new TaskProgressImplementation[Path](ProgressUnit.Bytes)
    val total: java.lang.Long = if (sizeHint.isDefined) sizeHint.get else null
    val bodyHandler =
      PathProgressBodyHandler.of(destination, taskProgress, total)
    asyncDownload(request, bodyHandler)
      .thenAccept(res => {
        if (res.statusCode() == 404) {
          taskProgress.setComplete(Failure(ResourceNotFound()))
          Files.deleteIfExists(destination)
        } else {
          taskProgress.setComplete(Success(destination))
        }
      })
      .exceptionally(ex => {
        taskProgress.setComplete(
          Failure(HTTPException(s"Server responded with: [${ex.getMessage}]."))
        )
        null
      })
    taskProgress
  }

  /** Downloads the `uri` and saves the response in the file pointed by the
    * `destination`.
    *
    * It is a shorthand for the other variant of [[download]] that creates a
    * simple GET request from the URI.
    *
    * @param uri the uri to download
    * @return a [[TaskProgress]] that tracks progress of the download and can
    *         be used to wait for the completion of the download.
    */
  def download(uri: URI, destination: Path): TaskProgress[Path] = {
    val request = HTTPRequestBuilder.fromURI(uri).GET
    download(request, destination)
  }

}
