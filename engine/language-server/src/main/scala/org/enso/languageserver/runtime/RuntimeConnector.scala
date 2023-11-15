package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, Props, Stash}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.RuntimeConnector.{
  Destroy,
  MessageFromRuntime
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.lockmanager.server.LockManagerService
import org.enso.logger.akka.ActorMessageLogging
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.{Api, ApiEnvelope}
import org.enso.profiling.events.EventsMonitor
import org.graalvm.polyglot.io.MessageEndpoint

import java.nio.ByteBuffer

/** An actor managing a connection to Enso's runtime server. */
final class RuntimeConnector(
  handlers: Map[Class[_], ActorRef],
  initialEventsMonitor: EventsMonitor
) extends Actor
    with LazyLogging
    with ActorMessageLogging
    with UnhandledLogging
    with Stash {

  override def preStart(): Unit = {
    logger.info("Starting the runtime connector.")
  }

  override def receive: Receive = {
    case RuntimeConnector.Initialize(engine) =>
      logger.info(
        s"Runtime connector established connection with the message endpoint [{}].",
        engine
      )
      unstashAll()
      context.become(waitingOnEndpoint(engine, initialEventsMonitor))
    case _ => stash()
  }

  private def waitingOnEndpoint(
    engine: MessageEndpoint,
    eventsMonitor: EventsMonitor
  ): Receive =
    registerEvent(eventsMonitor).andThen(LoggingReceive {
      case MessageFromRuntime(
            Runtime.Api.Response(None, Api.InitializedNotification())
          ) =>
        logger.debug(
          s"Message endpoint [{}] is initialized. Runtime connector can accept messages.",
          engine
        )
        unstashAll()
        context.become(initialized(engine, eventsMonitor, Map()))

      case RuntimeConnector.RegisterEventsMonitor(newEventsMonitor) =>
        eventsMonitor.close()
        context.become(waitingOnEndpoint(engine, newEventsMonitor))

      case _ => stash()
    })

  /** Performs communication between runtime and language server.
    *
    * Requests and responses can be sent in both directions and this Actor's
    * message queue is both receiving messages from the runtime's message
    * endpoint that it needs to forward to proper recipients as well as messages
    * sent from other Actors to itself that it needs to forward to the runtime.
    *
    * Since both sides of the connection can both send requests and responses,
    * the messages sent from the runtime are wrapped in [[MessageFromRuntime]],
    * so that the message queue can distinguish them.
    *
    * Messages from other Actors to the runtime are serialized and sent to the
    * [[MessageEndpoint]].
    *
    * Messages from the runtime are handled depending on their type. Responses
    * with a correlation id are sent to the Actor that sent the original request
    * (based on a mapping kept in the state). Other responses (mostly
    * notifications) are published to the system's event stream. Requests from
    * the runtime are forwarded to one of the registered handlers.
    *
    * @param engine  endpoint of a runtime
    * @param eventsMonitor the current events monitor
    * @param senders request ids with corresponding senders
    */
  def initialized(
    engine: MessageEndpoint,
    eventsMonitor: EventsMonitor,
    senders: Map[Runtime.Api.RequestId, ActorRef]
  ): Receive = registerEvent(eventsMonitor).andThen(LoggingReceive {
    case Destroy =>
      eventsMonitor.close()
      context.stop(self)

    case RuntimeConnector.RegisterEventsMonitor(newEventsMonitor) =>
      eventsMonitor.close()
      context.become(initialized(engine, newEventsMonitor, senders))

    case msg: Runtime.ApiEnvelope =>
      engine.sendBinary(Runtime.Api.serialize(msg))

      msg match {
        case Api.Request(Some(id), _) =>
          context.become(
            initialized(engine, eventsMonitor, senders + (id -> sender()))
          )
        case _ =>
      }

    case MessageFromRuntime(request @ Runtime.Api.Request(_, payload)) =>
      handlers.get(payload.getClass) match {
        case Some(handler) =>
          handler ! request
        case None =>
          logger.warn(
            s"No registered handler found for request " +
            s"[${payload.getClass.getCanonicalName}]."
          )
      }

    case MessageFromRuntime(Runtime.Api.Response(None, msg)) =>
      context.system.eventStream.publish(msg)

    case MessageFromRuntime(
          msg @ Runtime.Api.Response(Some(correlationId), payload)
        ) =>
      senders.get(correlationId) match {
        case Some(sender) =>
          sender ! msg
        case None =>
          logger.warn(
            "No sender has been found associated with request id [{}], the response [{}] will be dropped.",
            correlationId,
            payload.getClass.getCanonicalName
          )
      }
      context.become(
        initialized(engine, eventsMonitor, senders - correlationId)
      )
  })

  /** Register event in the events monitor
    *
    * @param eventsMonitor the current events monitor
    */
  private def registerEvent(
    eventsMonitor: EventsMonitor
  ): PartialFunction[Any, Any] = { event =>
    eventsMonitor.registerEvent(event)
    event
  }
}

object RuntimeConnector {

  /** Protocol message to pass the runtime connection to the actor.
    *
    * @param engineConnection the open runtime connection.
    */
  case class Initialize(engineConnection: MessageEndpoint)

  /** Protocol message to register new events monitor in the runtime connector.
    *
    * @param eventsMonitor the events monitor to register
    */
  case class RegisterEventsMonitor(eventsMonitor: EventsMonitor)

  /** Protocol message to inform the actor about the connection being closed.
    */
  case object Destroy

  /** Helper for creating instances of the [[RuntimeConnector]] actor.
    *
    * @param lockManagerService a reference to the lock manager service actor
    * @param monitor events monitor that handles messages between the language
    * server and the runtime
    * @return a [[Props]] instance for the newly created actor.
    */
  def props(lockManagerService: ActorRef, monitor: EventsMonitor): Props = {
    val lockRequests =
      LockManagerService.handledRequestTypes.map(_ -> lockManagerService)
    val handlers: Map[Class[_], ActorRef] = Map.from(lockRequests)
    Props(new RuntimeConnector(handlers, monitor))
  }

  /** Endpoint implementation used to handle connections with the runtime.
    *
    * @param actor        the actor ref to pass received messages to.
    * @param peerEndpoint the runtime server's connection end.
    */
  class Endpoint(actor: ActorRef, peerEndpoint: MessageEndpoint)
      extends MessageEndpoint {
    override def sendText(text: String): Unit = {}

    override def sendBinary(data: ByteBuffer): Unit =
      Runtime.Api
        .deserializeApiEnvelope(data)
        .foreach(actor ! MessageFromRuntime(_))

    override def sendPing(data: ByteBuffer): Unit = peerEndpoint.sendPong(data)

    override def sendPong(data: ByteBuffer): Unit = {}

    override def sendClose(): Unit = actor ! RuntimeConnector.Destroy
  }

  /** Wraps messages received from the runtime, to distinguish them from
    * messages received from other Actors.
    */
  case class MessageFromRuntime(message: ApiEnvelope)
}
