package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Status}
import akka.http.scaladsl.model.Uri
import akka.pattern.pipe
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult}
import org.enso.projectmanager.protocol.ProjectManagementApi.{
  LoggingServiceGetEndpoint,
  LoggingServiceUnavailable
}
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `logging-service/get-endpoint` commands.
  *
  * @param loggingServiceDescriptor a logging service configuration descriptor
  * @param requestTimeout timeout for the request
  */
class LoggingServiceEndpointRequestHandler(
  loggingServiceDescriptor: LoggingServiceDescriptor,
  requestTimeout: FiniteDuration
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  private def method = LoggingServiceGetEndpoint

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(LoggingServiceGetEndpoint, id, _) =>
      loggingServiceDescriptor.getEndpoint
        .map(LoggingServiceInitialized)
        .pipeTo(self)

      val timeoutCancellable = context.system.scheduler.scheduleOnce(
        requestTimeout,
        self,
        RequestTimeout
      )
      context.become(responseStage(id, sender(), timeoutCancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    timeoutCancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(ex, s"Failure during $method operation:")
      replyTo ! ResponseError(
        Some(id),
        LoggingServiceUnavailable(s"Logging service failed to set up: $ex")
      )
      timeoutCancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $method with $id timed out")
      replyTo ! ResponseError(
        Some(id),
        LoggingServiceUnavailable(
          "Logging service has not been set up within the timeout."
        )
      )
      context.stop(self)

    case LoggingServiceInitialized(maybeUri) =>
      maybeUri match {
        case Some(uri) =>
          replyTo ! ResponseResult(
            LoggingServiceGetEndpoint,
            id,
            LoggingServiceGetEndpoint.Result(uri.toString)
          )
        case None =>
          replyTo ! ResponseError(
            Some(id),
            LoggingServiceUnavailable("Logging service is not available.")
          )
      }
      timeoutCancellable.cancel()
      context.stop(self)
  }

  private case class LoggingServiceInitialized(endpoint: Option[Uri])
}

object LoggingServiceEndpointRequestHandler {

  /** Creates a configuration object used to create a
    * [[LoggingServiceEndpointRequestHandler]].
    *
    * @param loggingServiceDescriptor a logging service configuration descriptor
    * @param requestTimeout timeout for the request
    * @return a configuration object
    */
  def props(
    loggingServiceDescriptor: LoggingServiceDescriptor,
    requestTimeout: FiniteDuration
  ): Props = Props(
    new LoggingServiceEndpointRequestHandler(
      loggingServiceDescriptor,
      requestTimeout
    )
  )
}
