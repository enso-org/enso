package org.enso.interpreter.service;

import java.util.UUID;
import java.util.function.Consumer;
import org.enso.logger.ObservedMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class ExecutionProgressObserver implements Consumer<ObservedMessage> {
  private static final Logger PROGRESS = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private AutoCloseable handle = ObservedMessage.observe(PROGRESS, this);

  ExecutionProgressObserver() {}

  final void startComputation(UUID nodeId, Consumer<Double> c) {}

  @Override
  public void accept(ObservedMessage t) {
    System.err.println("seeing " + t.getMessage());
  }
}
