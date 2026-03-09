package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.ResourceBundle;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;

final class OtherJvmLogger extends System.LoggerFinder {
  private final Channel<OtherJvmPool> channel;
  private final Queue<LogMsg> pending = new ConcurrentLinkedQueue<>();

  OtherJvmLogger(Channel<OtherJvmPool> channel) {
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
    return new LoggerImpl(name);
  }

  private static System.Logger getRealSystemLogger(String n) {
    var log = System.getLogger(n);
    return log;
  }

  private static System.Logger.Level findLevel(int s) {
    return Stream.of(System.Logger.Level.values())
        .filter(l -> l.getSeverity() == s)
        .findAny()
        .get();
  }

  private void submitMsg(LogMsg log) {
    try {
      while (true) {
        var prev = pending.poll();
        if (prev == null) {
          break;
        }
        channel.execute(Void.class, prev);
      }
      channel.execute(Void.class, log);
    } catch (WrongThreadException ex) {
      // signals unability to call into the primary JVM
      pending.add(log);
    }
  }

  @Persistable(id = 81918)
  record LogCheck(String name, int severity) implements Function<Channel<OtherJvmPool>, Boolean> {

    @Override
    public Boolean apply(Channel<OtherJvmPool> t) {
      var log = getRealSystemLogger(name);
      var level = findLevel(severity());
      return log.isLoggable(level);
    }
  }

  @Persistable(id = 81919)
  record LogMsg(
      String name,
      int severity,
      List<String> format,
      List<? extends Object> args,
      List<OtherJvmResult<?, java.lang.Throwable>> thrown)
      implements Function<Channel<OtherJvmPool>, Void> {

    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      var log = getRealSystemLogger(name);
      var level = findLevel(severity());
      if (thrown.size() == 1) {
        assert args.isEmpty();
        try {
          thrown.get(0).value(null);
        } catch (Throwable ex) {
          log.log(level, formatMsg(), ex);
          return null;
        }
        assert false : "Should throw an exception: " + thrown;
      } else {
        log.log(level, formatMsg(), args.toArray());
      }
      return null;
    }

    private String formatMsg() {
      if (format.isEmpty()) {
        return null;
      } else {
        assert format.size() == 1;
        return format.get(0);
      }
    }
  }

  private final class LoggerImpl implements System.Logger {
    private final String name;
    private final Map<Level, Boolean> loggable =
        Collections.synchronizedMap(new EnumMap<>(Level.class));

    LoggerImpl(String name) {
      this.name = name;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public boolean isLoggable(Level level) {
      if ("org.enso.persist".equals(name)) {
        return level.compareTo(Level.INFO) >= 0;
      }
      var prev = loggable.get(level);
      if (prev != null) {
        return prev;
      }
      var check = new LogCheck(name, level.getSeverity());
      try {
        var b = channel.execute(Boolean.class, check);
        loggable.put(level, b);
        return b;
      } catch (WrongThreadException ex) {
        return true;
      }
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String msg, java.lang.Throwable thrown) {
      var ex = OtherJvmMessage.ThrowException.create(thrown);
      List<String> msgOpt = msg == null ? List.of() : List.of(msg);
      var log = new LogMsg(name, level.getSeverity(), msgOpt, List.of(), List.of(ex));
      submitMsg(log);
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String format, Object... params) {
      assert format != null;
      var validArgs = new ArrayList<Object>();
      if (params != null) {
        for (var p : params) {
          if (InteropLibrary.isValidProtocolValue(p)) {
            validArgs.add(p);
          } else {
            validArgs.add(p == null ? null : p.toString());
          }
        }
      }
      var log = new LogMsg(name, level.getSeverity(), List.of(format), validArgs, List.of());
      submitMsg(log);
    }
  }
}
