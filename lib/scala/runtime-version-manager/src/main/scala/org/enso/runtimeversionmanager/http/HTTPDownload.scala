package org.enso.runtimeversionmanager.http

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import com.typesafe.scalalogging.Logger
import org.enso.cli.task.{
  ProgressUnit,
  TaskProgress,
  TaskProgressImplementation
}

import scala.concurrent.Future

/** Represents a HTTP header. */
case class Header(name: String, value: String) {

  /** Checks if this header instance corresponds to a `headerName`.
    *
    * The check is case-insensitive.
    */
  def is(headerName: String): Boolean =
    name.toLowerCase == headerName.toLowerCase
}

/** Contains the response contents as a string alongside with the headers
  * included in the response.
  *
  * @param content the response decoded as a string
  * @param headers sequence of headers included in the response
  */
case class APIResponse(content: String, headers: Seq[Header])

/** Contains utility functions for fetching data using the HTTP(S) protocol.
  */
object HTTPDownload {
  private val logger = Logger[HTTPDownload.type]

  /** Determines how many redirects are taken until an error is thrown.
    */
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
    logger.debug("Fetching [{}].", request.requestImpl.getUri())
    def combineChunks(chunks: Seq[ByteString]): String =
      chunks.reduceOption(_ ++ _).map(_.decodeString(encoding)).getOrElse("")
    runRequest(
      request.requestImpl,
      sizeHint,
      Sink.seq,
      (response, chunks: Seq[ByteString]) =>
        APIResponse(
          combineChunks(chunks),
          response.headers.map(header => Header(header.name, header.value))
        )
    )
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
      request.requestImpl.getUri(),
      destination
    )
    runRequest(
      request.requestImpl,
      sizeHint,
      FileIO.toPath(destination),
      (_, _: Any) => destination
    )
  }

  implicit private lazy val actorSystem: ActorSystem = {
    val config = ConfigFactory
      .load()
      .withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("WARNING"))
    ActorSystem(
      "http-requests-actor-system",
      config
    )
  }

  /** Starts the request and returns a [[TaskProgress]] that can be used to
    * track download progress and get the result.
    *
    * @tparam A type of the result returned by `sink`
    * @tparam B type of the final result that will be contained in the returned
    *           [[TaskProgress]]
    * @param request  the request to start
    * @param sizeHint an optional hint indicating the expected size of the
    *                  response. It is used if the response does not include
    *                  explicit Content-Length header.
    * @param sink specifies how the response content should be handled, it
    *             receives chunks of [[ByteString]] and should produce a
    *             [[Future]] with some result
    * @param resultMapping maps the `sink` result and the response into a final
    *                      result type, it can use the response instance to for
    *                      example, access the headers, but the entity cannot be
    *                      used as it will already be drained
    * @return a [[TaskProgress]] that will contain the final result or any
    *         errors
    */
  private def runRequest[A, B](
    request: HttpRequest,
    sizeHint: Option[Long],
    sink: Sink[ByteString, Future[A]],
    resultMapping: (HttpResponse, A) => B
  ): TaskProgress[B] = {
    val taskProgress = new TaskProgressImplementation[B](ProgressUnit.Bytes)
    val total        = new java.util.concurrent.atomic.AtomicLong(0)
    import actorSystem.dispatcher

    val http = Http()

    def handleRedirects(retriesLeft: Int)(
      response: HttpResponse
    ): Future[HttpResponse] =
      if (response.status.isRedirection) {
        response.discardEntityBytes()
        if (retriesLeft > 0) {
          val newURI = response
            .header[Location]
            .getOrElse(
              throw HTTPException(
                s"HTTP response was ${response.status} which indicates a " +
                s"redirection, but the Location header was missing or invalid."
              )
            )
            .uri
          if (newURI.scheme != "https") {
            throw HTTPException(
              s"The HTTP redirection would result in a non-HTTPS connection " +
              s"(the requested scheme was `${newURI.scheme}`). " +
              "This is not safe, the request has been terminated."
            )
          }

          logger.trace(
            "HTTP response was [{}], redirecting to [{}].",
            response.status,
            newURI
          )
          val newRequest = request.withUri(newURI)
          http
            .singleRequest(newRequest)
            .flatMap(handleRedirects(retriesLeft - 1))
        } else {
          throw HTTPException("Too many redirects.")
        }
      } else Future.successful(response)

    def handleFinalResponse(
      response: HttpResponse
    ): Future[(HttpResponse, A)] = {
      val sizeEstimate =
        response.entity.contentLengthOption.orElse(sizeHint)
      response.entity.dataBytes
        .map { chunk =>
          val currentTotal = total.addAndGet(chunk.size.toLong)
          taskProgress.reportProgress(currentTotal, sizeEstimate)
          chunk
        }
        .runWith(sink)
        .map((response, _))
    }

    http
      .singleRequest(request)
      .flatMap(handleRedirects(maximumRedirects))
      .flatMap(handleFinalResponse)
      .map(resultMapping.tupled)
      .onComplete(taskProgress.setComplete)
    taskProgress
  }
}
