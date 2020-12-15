package org.enso.languageserver.requesthandler.monitoring

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.{
  Errors,
  Id,
  Request,
  ResponseError,
  ResponseResult,
  Unused
}
import org.enso.languageserver.monitoring.MonitoringApi
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

/** A request handler for `heartbeat/ping` commands.
  *
  * @param subsystems a list of monitored subsystems
  * @param timeout a request timeout
  */
class PingHandler(
  subsystems: List[ActorRef],
  timeout: FiniteDuration,
  shouldReplyWhenTimedOut: Boolean
) extends Actor
    with ActorLogging {

  import context.dispatcher

  private var cancellable: Option[Cancellable] = None

  override def receive: Receive = scatter

  private def scatter: Receive = {
    case Request(MonitoringApi.Ping, id, Unused) =>
      subsystems.foreach(_ ! Ping)
      cancellable = Some(
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      )
      context.become(gather(id, sender()))
  }

  private def gather(
    id: Id,
    replyTo: ActorRef,
    count: Int = 0
  ): Receive = {
    case RequestTimeout =>
      log.error(
        s"Health check timed out. Only $count/${subsystems.size} subsystems replied on time."
      )
      if (shouldReplyWhenTimedOut) {
        replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      }
      context.stop(self)

    case Pong =>
      if (count + 1 == subsystems.size) {
        replyTo ! ResponseResult(MonitoringApi.Ping, id, Unused)
        context.stop(self)
      } else {
        context.become(gather(id, replyTo, count + 1))
      }
  }

  override def postStop(): Unit = {
    cancellable.foreach(_.cancel())
  }

}

object PingHandler {

  /** Creates a configuration object used to create a [[PingHandler]]
    *
    * @param subsystems a list of monitored subsystems
    * @param timeout a request timeout
    * @return a configuration object
    */
  def props(
    subsystems: List[ActorRef],
    timeout: FiniteDuration,
    shouldReplyWhenTimedOut: Boolean = false
  ): Props =
    Props(new PingHandler(subsystems, timeout, shouldReplyWhenTimedOut))

}
