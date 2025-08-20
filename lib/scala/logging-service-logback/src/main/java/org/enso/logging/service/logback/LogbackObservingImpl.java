package org.enso.logging.service.logback;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.util.function.Consumer;
import org.enso.logger.ObservedMessage;
import org.slf4j.Logger;
import org.slf4j.event.Level;

public final class LogbackObservingImpl extends ObservedMessage.Service {

  @Override
  protected AutoCloseable observe(Logger logger, Consumer<ObservedMessage> observer) {
    if (logger instanceof ch.qos.logback.classic.Logger log) {
      var collector = new PassToConsumer(observer);
      collector.setContext(log.getLoggerContext());
      log.addAppender(collector);
      return collector;
    } else {
      return null;
    }
  }

  private final class PassToConsumer extends AppenderBase<ILoggingEvent> implements AutoCloseable {

    private final Consumer<ObservedMessage> observer;

    PassToConsumer(Consumer<ObservedMessage> observer) {
      this.observer = observer;
      this.start();
    }

    @Override
    protected void append(ILoggingEvent ev) {
      var level = findLevel(ev.getLevel());
      var record =
          newMessage(
              level,
              ev.getInstant(),
              ev.getMessage(),
              ev.getArgumentArray(),
              ev::getFormattedMessage);
      observer.accept(record);
    }

    private static Level findLevel(ch.qos.logback.classic.Level level) {
      if (ch.qos.logback.classic.Level.INFO == level) {
        return Level.INFO;
      }
      if (ch.qos.logback.classic.Level.DEBUG == level) {
        return Level.DEBUG;
      }
      if (ch.qos.logback.classic.Level.WARN == level) {
        return Level.WARN;
      }
      return Level.TRACE;
    }

    @Override
    public void close() throws Exception {
      stop();
    }
  }
}
