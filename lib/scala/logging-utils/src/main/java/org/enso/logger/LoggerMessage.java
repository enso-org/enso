package org.enso.logger;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.function.Consumer;
import org.slf4j.Logger;
import org.slf4j.event.Level;

public final class LoggerMessage {
  private final Level level;
  private final String msg;

  private LoggerMessage(Level level, String msg) {
    this.level = level;
    this.msg = msg;
  }

  public static AutoCloseable observe(Logger logger, Consumer<LoggerMessage> observer) {
    for (var observing : ServiceLoader.load(ObservingService.class)) {
      var handle = observing.observe(logger, observer);
      if (handle != null) {
        return handle;
      }
    }
    throw new IllegalStateException("No observing service found for " + logger);
  }

  public static List<LoggerMessage> collect(Logger logger, Runnable action) {
    var arr = new ArrayList<LoggerMessage>();
    try (var handle = observe(logger, arr::add)) {
      action.run();
    } catch (Exception ex) {
      throw new IllegalStateException(ex);
    }
    return arr;
  }

  public Level getLevel() {
    return level;
  }

  public String msg() {
    return msg;
  }

  public abstract static class ObservingService {
    protected abstract AutoCloseable observe(Logger logger, Consumer<LoggerMessage> observer);

    protected final LoggerMessage create(Level level, String msg) {
      return new LoggerMessage(level, msg);
    }
  }
}
