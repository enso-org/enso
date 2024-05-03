package org.enso.languageserver.util

import akka.actor.{Actor, ActorRef, Cancellable}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

abstract class HandlerWithRetries[
  RequestType: ClassTag,
  ResponseType: ClassTag
](runtime: ActorRef, timeout: FiniteDuration, retries: Int) {
  a: Actor with LazyLogging =>

  def this(runtime: ActorRef, timeout: FiniteDuration) = {
    this(runtime, timeout, 10)
  }

  import context.dispatcher

  override def receive: Receive = requestStage

  protected def request(msg: RequestType): Api.Request

  protected def requestStage: Receive = { case msg: RequestType =>
    val req = request(msg)
    runtime ! req
    val cancellable =
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
    context.become(responseStage(sender(), req, cancellable, retries))
  }

  private def responseStage(
    replyTo: ActorRef,
    request: Api.Request,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      logger.warn(
        "Failed to receive a [{}] response in [{}]. Retrying.",
        request,
        timeout
      )
      val newCancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(
        responseStage(replyTo, request, newCancellable, retries - 1)
      )

    case Api.Response(_, msg: ResponseType) =>
      positiveResponse(replyTo, msg)
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      negativeResponse(replyTo, error)
      cancellable.cancel()
      context.stop(self)
  }

  protected def positiveResponse(replyTo: ActorRef, msg: ResponseType): Unit
  protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(implicit
    ec: ExecutionContext
  ): Unit

}
