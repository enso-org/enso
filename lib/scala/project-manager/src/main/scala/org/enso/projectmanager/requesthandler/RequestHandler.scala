package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorRef, Cancellable, Stash, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.{LazyLogging, Logger}
import org.enso.cli.task.ProgressNotification
import org.enso.cli.task.notifications.ActorProgressNotificationForwarder
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.projectmanager.control.effect.Exec
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
  *                       to do in a general way.
  *                       If number of retry attempts is bigger than 0,
  *                       action will timeout and handler will stop only when the number
  *                       of retries reaches 0.
  */
abstract class RequestHandler[
  F[+_, +_]: Exec,
  FailureType: FailureMapper: Manifest,
  M <: Method,
  -Params,
  +Result
](
  method: M,
  requestTimeout: Option[FiniteDuration],
  numberOfTimeoutRetries: Int
)(implicit
  @unused evParams: HasParams.Aux[M, Params],
  @unused evResult: HasResult.Aux[M, Result]
) extends Actor
    with LazyLogging
    with Stash
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  def this(
    method: M,
    requestTimeout: Option[FiniteDuration]
  )(implicit
    @unused evParams: HasParams.Aux[M, Params],
    @unused evResult: HasResult.Aux[M, Result]
  ) = {
    this(method, requestTimeout, 0)
  }

  /** Waits for the request, tries to pass it into the [[handleRequest]]
    * function, sets up the timeout and routing of the result.
    */
  private def requestStage: Receive = {
    case request: Request[M, Params] @unchecked =>
      val result = handleRequest(request.params)
      Exec[F]
        .exec(result)
        .map(_.map(ResponseResult(method, request.id, _)))
        .pipeTo(self)
      val timeoutCancellable = {
        requestTimeout.map(timeout =>
          (
            context.system.scheduler.scheduleOnce(
              timeout,
              self,
              RequestTimeout
            ),
            numberOfTimeoutRetries
          )
        )
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
    timeoutCancellable: Option[(Cancellable, Int)]
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error(s"Failure during $method operation.", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      timeoutCancellable.foreach(_._1.cancel())
      context.stop(self)

    case RequestTimeout =>
      val retry = timeoutCancellable.map(_._2).getOrElse(0)
      if (retry == 0) {
        logger.error("Request {} with {} timed out.", method, id)
        replyTo ! ResponseError(Some(id), ServiceError)
        context.stop(self)
      } else {
        val retriesLeft = retry - 1
        val cancellable =
          requestTimeout.map(timeout =>
            (
              context.system.scheduler.scheduleOnce(
                timeout,
                self,
                RequestTimeout
              ),
              retriesLeft
            )
          )

        logger.warn(
          s"Pending $method response. Timeout retries left: {}.",
          retriesLeft
        )
        context.become(responseStage(id, replyTo, cancellable))
      }

    case Left(failure: FailureType) =>
      logger.error("Request {} with {} failed due to {}.", method, id, failure)
      val error = implicitly[FailureMapper[FailureType]].mapFailure(failure)
      replyTo ! ResponseError(Some(id), error)
      timeoutCancellable.foreach(_._1.cancel())
      context.stop(self)

    case Right(response) =>
      replyTo ! response
      timeoutCancellable.foreach(_._1.cancel())
      context.stop(self)

    case notification: ProgressNotification =>
      notification match {
        case ProgressNotification.TaskStarted(_, _, _) =>
          abandonTimeout(id, replyTo, timeoutCancellable)
        case _ =>
      }
      replyTo ! ActorProgressNotificationForwarder
        .translateProgressNotification(method.name, notification)
  }

  /** Cancels the timeout operation.
    *
    * Should be called when a long-running task is detected that we do not want
    * to interrupt.
    */
  private def abandonTimeout(
    id: Id,
    replyTo: ActorRef,
    timeoutCancellable: Option[(Cancellable, Int)]
  ): Unit = {
    timeoutCancellable.foreach { case (cancellable, _) =>
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
