package org.enso.interpreter.service;

import java.util.UUID;
import java.util.function.Consumer;
import org.enso.logger.ObservedMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class ExecutionProgressObserver implements Consumer<ObservedMessage> {
  private static final Logger PROGRESS = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private final UUID nodeId;
  private final Thread thread;
  private final AutoCloseable handle;
  private final Consumer<Double> consumer;

  ExecutionProgressObserver(UUID nodeId, Consumer<Double> c) {
    this.nodeId = nodeId;
    this.handle = ObservedMessage.observe(PROGRESS, this);
    this.thread = Thread.currentThread();
    this.consumer = c;
    // indeterminate computation has just started
    c.accept(-1.0);
    System.err.println("Observing for " + nodeId);
  }

  UUID nodeId() {
    return nodeId;
  }

  static ExecutionProgressObserver startComputation(UUID nodeId, Consumer<Double> c) {
    return new ExecutionProgressObserver(nodeId, c);
  }

  @Override
  public void accept(ObservedMessage t) {
    if (Thread.currentThread() == thread) {
      System.err.println("  seeing " + t.getMessage() + " for " + nodeId);
    }
  }

  final void finishComputation() {
    System.err.println("Finished computing " + nodeId);
    try {
      handle.close();
    } catch (Exception ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  private static <T extends Exception> T raise(Class<T> aClass, Exception ex) throws T {
    throw (T) ex;
  }
}
