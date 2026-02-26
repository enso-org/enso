package org.enso.jvm.interop.impl;

import java.util.List;
import java.util.ResourceBundle;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;

public final class OtherJvmLogger extends System.LoggerFinder {
  private final Channel<OtherJvmPool> channel;
  private static final ThreadLocal<Boolean> REAL_LOGGER = new ThreadLocal<>();

  public OtherJvmLogger(Channel<OtherJvmPool> channel) {
    this.channel = channel;
  }

  @SuppressWarnings("unchecked")
  static void initialize(Channel<OtherJvmPool> channel) {
    if (System.LoggerFinder.getLoggerFinder() instanceof Consumer delegatingFinder) {
      var logger = new OtherJvmLogger(channel);
      delegatingFinder.accept(logger);
    }
  }

  @Override
  public System.Logger getLogger(String name, Module module) {
    if (Boolean.TRUE.equals(REAL_LOGGER.get())) {
      return null;
    } else {
      return new LoggerImpl(name);
    }
  }

  private static System.Logger getRealSystemLogger(String n) {
    var prev = REAL_LOGGER.get();
    try {
      REAL_LOGGER.set(true);
      var log = System.getLogger(n);
      return log;
    } finally {
      REAL_LOGGER.set(prev);
    }
  }

  @Persistable(id = 81913)
  record LogMsg(String name, int severity, String format, List<Object> args, List<Throwable> thrown)
      implements Function<Channel<OtherJvmPool>, Void> {

    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      var log = getRealSystemLogger(name);
      var level =
          Stream.of(System.Logger.Level.values())
              .filter(l -> l.getSeverity() == severity)
              .findAny()
              .get();
      if (thrown.size() == 1) {
        assert args.isEmpty();
        log.log(level, format, thrown.get(0));
      } else {
        log.log(level, format, args.toArray());
      }
      return null;
    }
  }

  private final class LoggerImpl implements System.Logger {
    private final String name;

    LoggerImpl(String name) {
      this.name = name;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public boolean isLoggable(Level level) {
      return level.compareTo(Level.WARNING) >= 0;
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String msg, Throwable thrown) {
      var log = new LogMsg(name, level.getSeverity(), msg, List.of(), List.of(thrown));
      channel.execute(Void.class, log);
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String format, Object... params) {
      var log =
          new LogMsg(
              name,
              level.getSeverity(),
              format,
              params == null ? List.of() : List.of(params),
              List.of());
      channel.execute(Void.class, log);
    }
  }
}
