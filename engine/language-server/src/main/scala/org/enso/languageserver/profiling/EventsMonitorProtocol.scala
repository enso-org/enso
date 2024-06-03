package org.enso.languageserver.profiling

import org.enso.profiling.events.EventsMonitor

import java.nio.ByteBuffer

object EventsMonitorProtocol {

  /** Protocol message to register new events monitor in the runtime connector.
    *
    * @param eventsMonitor the events monitor to register
    */
  case class RegisterEventsMonitor(eventsMonitor: EventsMonitor)

  case class RegisterRuntimeMessage(message: Any)

  case class RegisterTextRpcMessage(message: String)

  case class RegisterBinaryRpcMessage(message: ByteBuffer)
}
