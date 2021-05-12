package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Stash, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.Logger
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{
  HasParams,
  HasResult,
  Id,
  Method,
  Request,
  ResponseError,
  ResponseResult
}
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.service.versionmanagement.ProgressNotification
import org.enso.projectmanager.service.versionmanagement.ProgressNotification.translateProgressNotification
import org.enso.projectmanager.util.UnhandledLogging

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

/** A helper class that gathers common request handling logic.
  *
  * It manages timeouts and sending the request result (in case of success but
  * also failure or timeout).
  *
  * @param method method that this handler deals with; used in logging
  *                      and to relate progress updates to the method
  * @param requestTimeout timeout for the request; if set, the request will be
  *                       marked as failed after the specified time; the request
  *                       logic is however NOT cancelled as this is not possible
  *                       to do in a general way
  */
abstract class RequestHandler[
  F[+_, +_]: Exec,
  FailureType: FailureMapper: Manifest,
  M <: Method,
  -Params,
  +Result
](
  method: M,
  requestTimeout: Option[FiniteDuration]
)(implicit
  @unused evParams: HasParams.Aux[M, Params],
  @unused evResult: HasResult.Aux[M, Result]
) extends Actor
    with ActorLogging
    with Stash
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  /** Waits for the request, tries to pass it into the [[handleRequest]]
    * function, sets up the timeout and routing of the result.
    */
  private def requestStage: Receive = { case request: Request[M, Params] =>
    val result = handleRequest(request.params)
    Exec[F]
      .exec(result)
      .map(_.map(ResponseResult(method, request.id, _)))
      .pipeTo(self)
    val timeoutCancellable = {
      requestTimeout.map { timeout =>
        context.system.scheduler.scheduleOnce(
          timeout,
          self,
          RequestTimeout
        )
      }
    }
    context.become(responseStage(request.id, sender(), timeoutCancellable))
  }

  /** Defines the actual logic for handling the request.
    *
    * The partial function should only be defined by requests that are meant to
    * be handled by this instance.
    */
  def handleRequest: Params => F[FailureType, Result]

  /** Waits for the routed result or a failure/timeout and reports the result to
    * the user.
    */
  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    timeoutCancellable: Option[Cancellable]
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(ex, "Failure during {} operation.", method)
      replyTo ! ResponseError(Some(id), ServiceError)
      timeoutCancellable.foreach(_.cancel())
      context.stop(self)

    case RequestTimeout =>
      log.error("Request {} with {} timed out.", method, id)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: FailureType) =>
      log.error("Request {} with {} failed due to {}.", method, id, failure)
      val error = implicitly[FailureMapper[FailureType]].mapFailure(failure)
      replyTo ! ResponseError(Some(id), error)
      timeoutCancellable.foreach(_.cancel())
      context.stop(self)

    case Right(response) =>
      replyTo ! response
      timeoutCancellable.foreach(_.cancel())
      context.stop(self)

    case notification: ProgressNotification =>
      notification match {
        case ProgressNotification.TaskStarted(_, _, _) =>
          abandonTimeout(id, replyTo, timeoutCancellable)
        case _ =>
      }
      replyTo ! translateProgressNotification(method.name, notification)
  }

  /** Cancels the timeout operation.
    *
    * Should be called when a long-running task is detected that we do not want
    * to interrupt.
    */
  private def abandonTimeout(
    id: Id,
    replyTo: ActorRef,
    timeoutCancellable: Option[Cancellable]
  ): Unit = {
    timeoutCancellable.foreach { cancellable =>
      cancellable.cancel()
      Logger[this.type].trace(
        "The operation {} ({}) reported starting a long-running task, " +
        "its request-timeout has been cancelled.",
        method,
        id
      )
    }
    context.become(responseStage(id, replyTo, None))
  }
}
