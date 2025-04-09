package org.enso.interpreter.service;

import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import org.enso.logger.ObservedMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** An observer watching for logged messages and converting them to progress notifications. */
final class ExecutionProgressObserver implements Consumer<ObservedMessage>, AutoCloseable {
  private static final Logger PROGRESS = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private final UUID nodeId;
  private final Thread thread;
  private final AutoCloseable handle;
  private final ProgressAggregator aggregate;

  private ExecutionProgressObserver(UUID nodeId, BiConsumer<Double, String> c) {
    this.nodeId = nodeId;
    this.handle = ObservedMessage.observe(PROGRESS, this);
    this.thread = Thread.currentThread();
    this.aggregate = new ProgressAggregator(c);
  }

  UUID nodeId() {
    return nodeId;
  }

  /**
   * Starts observing a computation of a node. Every time a progress is reported - either by
   * <em>updating the amount of finished work</em> or by logging a <em>text message</em> - the
   * {@code consumer}'s {@link BiConsumer#accept} method is invoked. The first number identifies the
   * progress (value less then zero means <em>indeterminate computation</em> is running) and the
   * <em>human readable detail</em> of the progress (may be {@code null}). Once the computation is
   * finished (by calling {@link AutoCloseable#close}) a final call to {@code accept(1.0, aMsg)} is
   * made signaling completion of the computation.
   *
   * @param nodeId the ID of the node to observe
   * @param consumer the consumer that receives the messages
   * @return a handle to call when the computation shall no longer be observed
   */
  static ExecutionProgressObserver startComputation(
      UUID nodeId, BiConsumer<Double, String> consumer) {
    var observer = new ExecutionProgressObserver(nodeId, consumer);
    // start by notifying indeterminate computation
    consumer.accept(-1.0, null);
    return observer;
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
            aggregate.log(key, msg);
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
            aggregate.log(key, msg);
          }
        }
        default -> {
          assert false : "Unexpected progress message: " + t.getMessage();
        }
      }
    }
  }

  @Override
  public void close() throws Exception {
    handle.close();
  }
}
