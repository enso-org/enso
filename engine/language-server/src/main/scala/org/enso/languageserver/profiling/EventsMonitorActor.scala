package org.enso.languageserver.profiling

import akka.actor.{Actor, ActorRef, Props}
import org.enso.jsonrpc.MessageHandler
import org.enso.languageserver.util.UnhandledLogging
import org.enso.profiling.events.EventsMonitor

import java.nio.ByteBuffer

final class EventsMonitorActor(initialEventsMonitor: EventsMonitor)
    extends Actor
    with UnhandledLogging {

  override def receive: Receive =
    initialized(initialEventsMonitor)

  private def initialized(eventsMonitor: EventsMonitor): Receive = {
    case EventsMonitorProtocol.RegisterEventsMonitor(newEventsMonitor) =>
      context.become(initialized(newEventsMonitor))

    case EventsMonitorProtocol.RegisterRuntimeMessage(message) =>
      eventsMonitor.registerRuntimeMessage(message)

    case EventsMonitorProtocol.RegisterTextRpcMessage(message) =>
      eventsMonitor.registerTextRpcMessage(message)

    case EventsMonitorProtocol.RegisterBinaryRpcMessage(message) =>
      eventsMonitor.registerBinaryRpcMessage(message)
  }
}

object EventsMonitorActor {

  def props(eventsMonitor: EventsMonitor): Props =
    Props(new EventsMonitorActor(eventsMonitor))

  def messagesCallback(eventsMonitor: ActorRef): Actor.Receive = {
    case webMessage: MessageHandler.WebMessage =>
      eventsMonitor ! EventsMonitorProtocol.RegisterTextRpcMessage(
        webMessage.message
      )

    case byteBuffer: ByteBuffer =>
      eventsMonitor ! EventsMonitorProtocol.RegisterBinaryRpcMessage(byteBuffer)
  }
}
