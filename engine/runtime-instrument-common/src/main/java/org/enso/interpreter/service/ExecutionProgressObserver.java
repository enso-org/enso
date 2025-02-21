package org.enso.interpreter.service;

import java.util.UUID;
import java.util.function.Consumer;
import org.enso.logger.LoggerMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class ExecutionProgressObserver implements Consumer<LoggerMessage> {
  private static final Logger PROGRESS = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private AutoCloseable handle = LoggerMessage.observe(PROGRESS, this);

  ExecutionProgressObserver() {}

  final void startComputation(UUID nodeId, Consumer<Double> c) {}

  @Override
  public void accept(LoggerMessage t) {
    System.err.println("seeing " + t.msg());
  }
}
