package org.enso.languageserver.io

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.event.{
  ExecutionContextCreated,
  ExecutionContextDestroyed,
  ExecutionContextEvent
}
import org.enso.languageserver.io.InputOutputProtocol.{
  FeedStandardInput,
  WaitingForStandardInput
}
import org.enso.languageserver.io.InputRedirectionController.ContextData
import org.enso.languageserver.io.ObservablePipedInputStream.{
  InputObserver,
  InputStreamEvent,
  ReadBlocked
}
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.languageserver.util.UnhandledLogging

/** A stdin redirection controller responsible for feeding stdin and notifying
  * execution context owners that executed program is blocked on `IO.readln`.
  *
  * @param stdIn a stream from which programs read its input data
  * @param stdInSink     a sink used to feed stdin
  * @param sessionRouter used to deliver stdin updates to context owners
  */
class InputRedirectionController(
  stdIn: ObservablePipedInputStream,
  stdInSink: ObservableOutputStream,
  sessionRouter: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging
    with InputObserver {

  override def preStart(): Unit = {
    stdIn.attach(this)
    context.system.eventStream.subscribe(self, classOf[ExecutionContextEvent])
  }

  override def receive: Receive = running()

  private def running(liveContexts: Set[ContextData] = Set.empty): Receive = {
    case FeedStandardInput(input, isLineTerminated) =>
      logger.debug("Feeding stdin [{} bytes]", input.length)
      if (isLineTerminated) {
        val bytes =
          ByteString.createBuilder
            .append(ByteString.fromArray(input.getBytes))
            .append(ByteString.fromArray(System.lineSeparator().getBytes))
            .result()

        stdInSink.write(bytes.toArray)
      } else {
        stdInSink.write(input.getBytes)
      }

    case ExecutionContextCreated(contextId, owner) =>
      context.become(running(liveContexts + ContextData(contextId, owner)))

    case ExecutionContextDestroyed(contextId, owner) =>
      context.become(running(liveContexts - ContextData(contextId, owner)))

    case ReadBlocked =>
      logger.debug("Blocked read detected.")
      liveContexts foreach { case ContextData(_, owner) =>
        sessionRouter ! DeliverToJsonController(
          owner,
          WaitingForStandardInput
        )
      }
  }

  /** @inheritdoc */
  override def update(event: InputStreamEvent): Unit = { self ! event }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    stdIn.detach(this)
  }

  override def postStop(): Unit = {
    stdIn.detach(this)
  }

}

object InputRedirectionController {

  private case class ContextData(contextId: UUID, owner: ClientId)

  /** Creates a configuration object used to create a
    * [[InputRedirectionController]].
    *
    * @param stdIn a stream from which programs read its input data
    * @param stdInSink     a sink used to feed stdin
    * @param sessionRouter used to deliver stdin updates to context owners
    * @return a configuration object
    */
  def props(
    stdIn: ObservablePipedInputStream,
    stdInSink: ObservableOutputStream,
    sessionRouter: ActorRef
  ): Props =
    Props(new InputRedirectionController(stdIn, stdInSink, sessionRouter))

}
