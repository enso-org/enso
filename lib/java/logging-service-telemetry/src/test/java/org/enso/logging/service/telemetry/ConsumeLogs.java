package org.enso.logging.service.telemetry;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.util.ArrayList;
import java.util.List;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A custom jUnit {@link TestRule} that captures all the logs and prints them only if there is a
 * test failure.
 */
public final class ConsumeLogs implements TestRule {
  private static final Level LOG_LEVEL = Level.TRACE;
  private final MemoryAppender appender = new MemoryAppender();

  public ConsumeLogs() {
    var context = (LoggerContext) LoggerFactory.getILoggerFactory();
    appender.setContext(context);
    appender.setName("MemoryAppender");
    var rootLogger = context.getLogger(Logger.ROOT_LOGGER_NAME);
    rootLogger.detachAndStopAllAppenders();
    rootLogger.addAppender(appender);
    rootLogger.setLevel(LOG_LEVEL);
    appender.start();
  }

  @Override
  public Statement apply(Statement statement, Description description) {
    return new CustomStatement(statement, description);
  }

  private final class CustomStatement extends Statement {
    private final Statement base;
    private final Description description;

    private CustomStatement(Statement base, Description description) {
      this.base = base;
      this.description = description;
    }

    @Override
    public void evaluate() throws Throwable {
      try {
        base.evaluate();
      } catch (Throwable e) {
        System.err.println(
            "Test "
                + description.getDisplayName()
                + " failed with "
                + e.getClass().getName()
                + ". Captured output from the logs:");
        for (var event : appender.getEvents()) {
          System.err.println("  " + event.getFormattedMessage());
        }
        throw e;
      }
    }
  }

  private static final class MemoryAppender extends AppenderBase<ILoggingEvent> {
    private final List<ILoggingEvent> events = new ArrayList<>();

    public List<ILoggingEvent> getEvents() {
      return events;
    }

    @Override
    protected void append(ILoggingEvent eventObject) {
      events.add(eventObject);
    }
  }
}
