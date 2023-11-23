package org.enso.languageserver.requesthandler.profiling

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.profiling.{ProfilingApi, ProfilingProtocol}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `profiling/start` commands.
  *
  * @param timeout a request timeout
  * @param profilingManager a reference to the profiling manager
  */
class ProfilingStartHandler(timeout: FiniteDuration, profilingManager: ActorRef)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ProfilingApi.ProfilingStart, id, _) =>
      profilingManager ! ProfilingProtocol.ProfilingStartRequest
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(
        responseStage(
          id,
          sender(),
          cancellable
        )
      )
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case ProfilingProtocol.ProfilingStartResponse =>
      replyTo ! ResponseResult(ProfilingApi.ProfilingStart, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }

}

object ProfilingStartHandler {

  /** Creates configuration object used to create a [[ProfilingStartHandler]].
    *
    * @param timeout request timeout
    * @param profilingManager reference to the profiling manager
    */
  def props(timeout: FiniteDuration, profilingManager: ActorRef): Props =
    Props(new ProfilingStartHandler(timeout, profilingManager))

}
