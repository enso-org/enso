package org.enso.interpreter.instrument.id.execution;

import java.util.function.Consumer;
import org.enso.logger.LoggerMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class IdExecutionProgressObserver implements Consumer<LoggerMessage> {
  private static final IdExecutionProgressObserver INSTANCE = new IdExecutionProgressObserver();
  private static final Logger PROGRESS = LoggerFactory.getLogger("Standard.Base.Logging.Progress");
  private static final AutoCloseable HANDLE = LoggerMessage.observe(PROGRESS, INSTANCE);

  private IdExecutionProgressObserver() {}

  @Override
  public void accept(LoggerMessage t) {
    System.err.println("seeing " + t.msg());
  }
}
