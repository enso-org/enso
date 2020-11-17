package org.enso.languageserver.requesthandler.monitoring

import akka.actor.{Actor, ActorLogging, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult, Unused}
import org.enso.languageserver.monitoring.MonitoringApi

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/** A request handler for `heartbeat/init` commands.
  *
  * @param initializationFuture a future that is completed when initialization
  *                             is finished
  */
class InitializationNotificationHandler(initializationFuture: Future[_])
    extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = {
    case Request(MonitoringApi.InitializationNotify, id, Unused) =>
      initializationFuture.onComplete {
        case Failure(_) =>
          sender() ! ResponseError(Some(id), ServiceError)
        case Success(_) =>
          sender() ! ResponseResult(
            MonitoringApi.InitializationNotify,
            id,
            Unused
          )
      }
  }

}

object InitializationNotificationHandler {

  /** Creates a configuration object used to create a
    * [[InitializationNotificationHandler]]
    *
    * @param initializationFuture a future that is completed when initialization
    *                             is finished
    * @param timeout a request timeout
    * @return a configuration object
    */
  def props(initializationFuture: Future[_], timeout: FiniteDuration): Props =
    Props(new InitializationNotificationHandler(initializationFuture, timeout))

}
