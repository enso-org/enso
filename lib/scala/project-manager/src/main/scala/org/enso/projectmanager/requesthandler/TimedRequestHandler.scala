package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Stash, Status}
import akka.pattern.pipe
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Id, Request, ResponseError}
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectList
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

abstract class TimedRequestHandler[
  F[+_, +_]: Exec,
  FailureType: FailureMapper: Manifest
](
  requestTimeout: FiniteDuration
) extends Actor
    with ActorLogging
    with Stash
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  def requestStage: Receive = {
    val composition: Any => Option[Unit] = {
      case request @ Request(_, id, _) =>
        val result = handleRequest.lift(request)
        result.map { f =>
          Exec[F].exec(f).pipeTo(self)
          val cancellable =
            context.system.scheduler
              .scheduleOnce(requestTimeout, self, RequestTimeout)
          context.become(responseStage(id, sender(), cancellable))
        }
      case _ => None
    }
    Function.unlift(composition)
  }

  def handleRequest: PartialFunction[Any, F[FailureType, Any]]

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(ex, s"Failure during $ProjectList operation:")
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $ProjectList with $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: FailureType) =>
      log.error(s"Request $id failed due to $failure")
      val error = implicitly[FailureMapper[FailureType]].mapFailure(failure)
      replyTo ! ResponseError(Some(id), error)
      cancellable.cancel()
      context.stop(self)

    case Right(response) =>
      replyTo ! response
      cancellable.cancel()
      context.stop(self)
  }
}
