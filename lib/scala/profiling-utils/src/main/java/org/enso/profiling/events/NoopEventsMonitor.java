package org.enso.profiling.events;

import java.io.IOException;
import java.nio.ByteBuffer;

/** Events monitor that does nothing. */
final class NoopEventsMonitor implements EventsMonitor {

  @Override
  public void registerRuntimeMessage(Object event) {}

  @Override
  public void registerTextRpcMessage(String message) {}

  @Override
  public void registerBinaryRpcMessage(ByteBuffer message) {}

  @Override
  public void close() throws IOException {}
}
