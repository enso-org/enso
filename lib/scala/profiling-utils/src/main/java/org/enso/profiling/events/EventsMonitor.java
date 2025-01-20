package org.enso.profiling.events;

import java.io.Closeable;
import java.nio.ByteBuffer;

/** Diagnostic tool that processes event messages. Used for debugging or performance review. */
public interface EventsMonitor extends Closeable {

  /**
   * Process the event message.
   *
   * @param event the event to register.
   */
  void registerRuntimeMessage(Object event);

  void registerTextRpcMessage(String message);

  void registerBinaryRpcMessage(ByteBuffer message);
}
