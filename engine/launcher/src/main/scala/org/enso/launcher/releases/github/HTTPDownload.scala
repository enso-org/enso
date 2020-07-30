package org.enso.launcher.releases.github

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpClient.Redirect
import java.net.http.HttpResponse.{BodyHandler, BodyHandlers}
import java.nio.ByteBuffer
import java.nio.file.Path
import java.util
import java.util.concurrent.{CompletionStage, Flow}

import scala.jdk.CollectionConverters._
import org.enso.cli.{ProgressListener, TaskProgress}

import scala.util.{Failure, Success, Try}

class HTTPDownload[A] private[github] (
  client: HttpClient,
  request: HttpRequest,
  baseHandler: BodyHandler[A],
  sizeHint: Option[Long]
) extends TaskProgress[A] {

  private var listeners: List[ProgressListener[A]] = Nil
  private var result: Option[Try[A]]               = None
  start()

  override def addProgressListener(
    listener: ProgressListener[A]
  ): Unit = {
    this.synchronized {
      result match {
        case Some(value) =>
          listener.done(value)
        case None =>
      }

      listeners ::= listener
    }
  }

  private[this] def start(): Unit = {
    val response =
      client.sendAsync(request, new ProgressHandlerWrapper(baseHandler))
    response.whenCompleteAsync { (response, error) =>
      val result =
        if (error != null) Failure(error) else Success(response.body())
      setComplete(result)
    }
  }

  private def setComplete(result: Try[A]): Unit = {
    this.synchronized {
      this.result = Some(result)
      listeners.foreach(_.done(result))
    }
  }

  private def reportProgress(done: Long, total: Option[Long]): Unit = {
    listeners.foreach(_.progressUpdate(done, total))
  }

  private class ProgressHandlerWrapper(originalHandler: BodyHandler[A])
      extends BodyHandler[A] {
    override def apply(
      responseInfo: HttpResponse.ResponseInfo
    ): HttpResponse.BodySubscriber[A] = {
      val contentLength =
        responseInfo.headers().firstValueAsLong("Content-Length")
      val size =
        if (contentLength.isPresent) Some(contentLength.getAsLong) else sizeHint
      new ProgressHandlerSubscriberWrapper(
        size,
        originalHandler.apply(responseInfo)
      )
    }
  }

  private class ProgressHandlerSubscriberWrapper(
    totalSize: Option[Long],
    originalSubscriber: HttpResponse.BodySubscriber[A]
  ) extends HttpResponse.BodySubscriber[A] {
    private var done: Long = 0
    override def getBody: CompletionStage[A] = {
      originalSubscriber.getBody
    }

    override def onSubscribe(subscription: Flow.Subscription): Unit = {
      originalSubscriber.onSubscribe(subscription)
    }

    override def onNext(item: util.List[ByteBuffer]): Unit = {
      done += item.asScala.map(_.remaining()).sum
      originalSubscriber.onNext(item)
      reportProgress(done, totalSize)
    }

    override def onError(throwable: Throwable): Unit = {
      originalSubscriber.onError(throwable)
    }

    override def onComplete(): Unit = {
      originalSubscriber.onComplete()
    }
  }

}

object HTTPDownload {
  private lazy val client =
    HttpClient.newBuilder().followRedirects(Redirect.NORMAL).build()

  def fetchString(
    request: HttpRequest,
    sizeHint: Option[Long] = None
  ): HTTPDownload[String] = {
    new HTTPDownload[String](client, request, BodyHandlers.ofString(), sizeHint)
  }

  def download(
    request: HttpRequest,
    destination: Path,
    sizeHint: Option[Long] = None
  ): HTTPDownload[Path] = {
    new HTTPDownload[Path](
      client,
      request,
      BodyHandlers.ofFile(destination),
      sizeHint
    )
  }
}
