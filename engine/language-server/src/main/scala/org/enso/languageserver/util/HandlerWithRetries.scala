package org.enso.languageserver.util

import akka.actor.{Actor, ActorRef, Cancellable}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

/** Handler base class with retries support.
  *
  * @tparam RequestType type of the message being handled
  * @tparam ResponseType type of the message expected when message is successfully handled
  * @tparam ErrorType type of the message expected when message has failed to be handled
  * @tparam ForwardedPayloadType type of the message forwarded to `target`
  */
abstract class HandlerWithRetries[
  RequestType,
  ResponseType,
  ErrorType,
  ForwardedPayloadType
] {
  a: Actor with LazyLogging =>

  override def receive: Receive = requestStage

  protected def request(msg: RequestType): ForwardedPayloadType

  protected def requestStage: Receive
}

/** API handler base class with retries support.
  *
  * @param target target actor which will handle a message
  * @param timeout timeout for serving a message
  * @param retries number of retries attempted on timeout before aborting
  * @tparam RequestType type of the message being handled
  * @tparam ResponseType type of the message expected when message is successfully handled
  */
abstract class ApiHandlerWithRetries[
  RequestType: ClassTag,
  ResponseType: ClassTag
](target: ActorRef, timeout: FiniteDuration, retries: Int)
    extends HandlerWithRetries[
      RequestType,
      ResponseType,
      Api.Error,
      Api.Request
    ] {
  a: Actor with LazyLogging =>

  def this(runtime: ActorRef, timeout: FiniteDuration) = {
    this(runtime, timeout, 10)
  }

  import context.dispatcher

  override def receive: Receive = requestStage

  protected def request(msg: RequestType): Api.Request

  protected def requestStage: Receive = { case msg: RequestType =>
    val req = request(msg)
    target ! req
    val cancellable =
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
    context.become(responseStage(sender(), req, cancellable, retries))
  }

  private def responseStage(
    replyTo: ActorRef,
    forwardedRequest: Api.Request,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      if (retries > 0) {
        logger.warn(
          "Failed to receive a [{}] response in [{}]. Retrying.",
          forwardedRequest,
          timeout
        )
        val newCancellable =
          context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
        context.become(
          responseStage(
            replyTo,
            forwardedRequest,
            newCancellable,
            retries - 1
          )
        )
      } else {
        replyTo ! RequestTimeout
        context.stop(self)
      }

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

/** Request handler base class with retries support.
  *
  * @param target target actor which will handle a message
  * @param timeout timeout for serving a message
  * @param retries number of retries attempted on timeout before aborting
  * @tparam RequestType type of the message being handled
  * @tparam ResponseType type of the message expected when message is successfully handled
  * @tparam ErrorType type of the message expected when message has failed to be handled
  * @tparam ForwardedPayloadType type of the message forwarded to `target`
  */
abstract class RequestHandlerWithRetries[
  RequestType: ClassTag,
  ResponseType: ClassTag,
  ErrorType: ClassTag,
  ForwardedPayloadType
](target: ActorRef, timeout: FiniteDuration, retries: Int)
    extends HandlerWithRetries[
      RequestType,
      ResponseType,
      ErrorType,
      ForwardedPayloadType
    ] {
  a: Actor with LazyLogging =>

  def this(runtime: ActorRef, timeout: FiniteDuration) = {
    this(runtime, timeout, 10)
  }

  import context.dispatcher

  override def receive: Receive = requestStage

  protected def request(msg: RequestType): ForwardedPayloadType

  protected def requestStage: Receive = { case msg: RequestType =>
    val req = request(msg)
    target ! req
    val cancellable =
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
    context.become(responseStage(sender(), msg, req, cancellable, retries))
  }

  private def responseStage(
    replyTo: ActorRef,
    initialMsg: RequestType,
    forwardedRequest: ForwardedPayloadType,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      if (retries > 0) {
        logger.warn(
          "Failed to receive a [{}] response in [{}]. Retrying.",
          forwardedRequest,
          timeout
        )
        val newCancellable =
          context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
        context.become(
          responseStage(
            replyTo,
            initialMsg,
            forwardedRequest,
            newCancellable,
            retries - 1
          )
        )
      } else {
        replyTo ! RequestTimeout
        context.stop(self)
      }

    case msg: ResponseType =>
      positiveResponse(replyTo, initialMsg, msg)
      cancellable.cancel()
      context.stop(self)

    case error: ErrorType =>
      negativeResponse(replyTo, initialMsg, error)
      cancellable.cancel()
      context.stop(self)

  }

  protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: RequestType,
    msg: ResponseType
  ): Unit

  protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: RequestType,
    error: ErrorType
  )(implicit
    ec: ExecutionContext
  ): Unit

}

/** Request handler base class with retries support. Handler forwards messages directly to runtime.
  *
  * @param target target actor which will handle a message
  * @param timeout timeout for serving a message
  * @param retries number of retries attempted on timeout before aborting
  * @tparam RequestType type of the message being handled
  * @tparam ResponseType type of the message expected when message is successfully handled
  * @tparam ErrorType type of the message expected when message has failed to be handled
  * @tparam ForwardedPayloadType type of the message forwarded to `target`
  */
abstract class RequestToApiHandlerWithRetries[
  RequestType: ClassTag,
  ResponseType: ClassTag,
  ErrorType: ClassTag,
  ForwardedPayloadType
](target: ActorRef, timeout: FiniteDuration, retries: Int)
    extends HandlerWithRetries[
      RequestType,
      ResponseType,
      ErrorType,
      ForwardedPayloadType
    ] {
  a: Actor with LazyLogging =>

  def this(runtime: ActorRef, timeout: FiniteDuration) = {
    this(runtime, timeout, 10)
  }

  import context.dispatcher

  override def receive: Receive = requestStage

  protected def request(msg: RequestType): ForwardedPayloadType

  protected def requestStage: Receive = { case msg: RequestType =>
    val req = request(msg)
    target ! req
    val cancellable =
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
    context.become(responseStage(sender(), msg, req, cancellable, retries))
  }

  private def responseStage(
    replyTo: ActorRef,
    initialMsg: RequestType,
    forwardedRequest: ForwardedPayloadType,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      if (retries > 0) {
        logger.warn(
          "Failed to receive a [{}] response in [{}]. Retrying.",
          forwardedRequest,
          timeout
        )
        val newCancellable =
          context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
        context.become(
          responseStage(
            replyTo,
            initialMsg,
            forwardedRequest,
            newCancellable,
            retries - 1
          )
        )
      } else {
        replyTo ! RequestTimeout
        context.stop(self)
      }

    case Api.Response(_, msg: ResponseType) =>
      positiveResponse(replyTo, initialMsg, msg)
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: ErrorType) =>
      negativeResponse(replyTo, initialMsg, error)
      cancellable.cancel()
      context.stop(self)

  }

  protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: RequestType,
    msg: ResponseType
  ): Unit

  protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: RequestType,
    error: ErrorType
  )(implicit
    ec: ExecutionContext
  ): Unit

}
