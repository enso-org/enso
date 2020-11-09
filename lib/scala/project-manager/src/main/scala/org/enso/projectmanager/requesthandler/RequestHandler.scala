package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Stash, Status}
import akka.pattern.pipe
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Id, Method, Notification, Request, ResponseError}
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.{
  TaskFinished,
  TaskProgressUpdate,
  TaskStarted
}
import org.enso.projectmanager.service.versionmanagement.ProgressNotification
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

abstract class RequestHandler[
  F[+_, +_]: Exec,
  FailureType: FailureMapper: Manifest
](
  handledMethod: Method,
  requestTimeout: Option[FiniteDuration]
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
          val cancellable = {
            requestTimeout.map { timeout =>
              context.system.scheduler.scheduleOnce(
                timeout,
                self,
                RequestTimeout
              )
            }
          }
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
    cancellable: Option[Cancellable]
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(ex, s"Failure during $handledMethod operation:")
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.foreach(_.cancel())
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $handledMethod with $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: FailureType) =>
      log.error(s"Request $id failed due to $failure")
      val error = implicitly[FailureMapper[FailureType]].mapFailure(failure)
      replyTo ! ResponseError(Some(id), error)
      cancellable.foreach(_.cancel())
      context.stop(self)

    case Right(response) =>
      replyTo ! response
      cancellable.foreach(_.cancel())
      context.stop(self)

    case notification: ProgressNotification =>
      replyTo ! translateProgressNotification(notification)
  }

  private def translateProgressNotification(
    progressNotification: ProgressNotification
  ): Notification[_, _] = progressNotification match {
    case ProgressNotification.TaskStarted(taskId, total, unit) =>
      Notification(
        TaskStarted,
        TaskStarted.Params(
          taskId           = taskId,
          relatedOperation = handledMethod.name,
          unit             = unit,
          total            = total
        )
      )
    case ProgressNotification.TaskUpdate(taskId, message, done) =>
      Notification(
        TaskProgressUpdate,
        TaskProgressUpdate.Params(taskId, message, done)
      )
    case ProgressNotification.TaskSuccess(taskId) =>
      Notification(
        TaskFinished,
        TaskFinished.Params(taskId, None, success = true)
      )
    case ProgressNotification.TaskFailure(taskId, throwable) =>
      Notification(
        TaskFinished,
        TaskFinished.Params(taskId, Some(throwable.getMessage), success = false)
      )
  }
}
