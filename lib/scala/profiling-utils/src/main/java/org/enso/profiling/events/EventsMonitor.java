package org.enso.profiling.events;

import java.io.Closeable;

/** Diagnostic tool that processes event messages. Used for debugging or performance review. */
public interface EventsMonitor extends Closeable {

  /**
   * Process the event message.
   *
   * @param event the event to register.
   */
  void registerEvent(Object event);
}
