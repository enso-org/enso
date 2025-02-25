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
  private final ProgressAggregator aggregate;

  ExecutionProgressObserver(UUID nodeId, Consumer<Double> c) {
    this.nodeId = nodeId;
    this.handle = ObservedMessage.observe(PROGRESS, this);
    this.thread = Thread.currentThread();
    this.aggregate = new ProgressAggregator(c);
    // start by notifying indeterminate computation
    c.accept(-1.0);
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
      switch (t.getMessage()) {
        case "INIT {}:{}@{}" -> {
          if (t.getArguments().size() >= 3
              && t.getArguments().get(1) instanceof String msg
              && t.getArguments().get(2) instanceof Number max) {
            var key = t.getArguments().get(0);
            aggregate.create(key, max.longValue());
          }
        }
        case "ADVANCE {}+{}" -> {
          if (t.getArguments().size() >= 2 && t.getArguments().get(1) instanceof Number by) {
            var key = t.getArguments().get(0);
            aggregate.advanceBy(key, by.longValue());
          }
        }
        case "LOG {}:{}" -> {
          if (t.getArguments().size() >= 2 && t.getArguments().get(1) instanceof String msg) {
            var key = t.getArguments().get(0);
            // TBD: now what?
          }
        }
        default -> {
          assert false : "Unexpected progress message: " + t.getMessage();
        }
      }
    }
  }

  final void finishComputation() {
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
