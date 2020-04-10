package org.enso.languageserver.requesthandler.monitoring

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.{Id, Request, ResponseResult, Unused}
import org.enso.languageserver.monitoring.MonitoringApi
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `heartbeat/ping` commands.
  *
  * @param subsystems a list of monitored subsystems
  * @param timeout a request timeout
  */
class PingHandler(
  subsystems: List[ActorRef],
  timeout: FiniteDuration
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = scatter

  private def scatter: Receive = {
    case Request(MonitoringApi.Ping, id, Unused) =>
      subsystems.foreach(_ ! Ping)
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(gather(id, sender(), cancellable))
  }

  private def gather(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable,
    count: Int = 0
  ): Receive = {
    case RequestTimeout =>
      log.error(
        s"Health check timed out. Only $count/${subsystems.size} subsystems replied on time."
      )
      context.stop(self)

    case Pong =>
      if (count + 1 == subsystems.size) {
        replyTo ! ResponseResult(MonitoringApi.Ping, id, Unused)
        cancellable.cancel()
        context.stop(self)
      } else {
        context.become(gather(id, replyTo, cancellable, count + 1))
      }
  }

}

object PingHandler {

  /**
    * Creates a configuration object used to create a [[PingHandler]]
    *
    * @param subsystems a list of monitored subsystems
    * @param timeout a request timeout
    * @return a configuration object
    */
  def props(subsystems: List[ActorRef], timeout: FiniteDuration): Props =
    Props(new PingHandler(subsystems, timeout))

}
