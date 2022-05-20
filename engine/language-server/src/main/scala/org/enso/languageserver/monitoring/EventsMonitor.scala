package org.enso.languageserver.monitoring

/** Diagnostic tool that processes event messages. Used for debugging or
  * performance review.
  */
trait EventsMonitor {

  /** Process the event message.
    *
    * @param event the event message
    */
  def registerEvent(event: Any): Unit
}

/** Events monitor that does nothing. */
final class NoopEventsMonitor extends EventsMonitor {

  /** @inheritdoc */
  override def registerEvent(event: Any): Unit = ()
}
