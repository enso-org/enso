package org.enso.profiling.events;

import java.io.IOException;

/** Events monitor that does nothing. */
public final class NoopEventsMonitor implements EventsMonitor {

  @Override
  public void registerEvent(Object event) {}

  @Override
  public void close() throws IOException {}
}
