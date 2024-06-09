package org.enso.interpreter.test;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

public final class MockLogHandler extends Handler {
  private final List<LogRecord> logs = new ArrayList<>();

  public MockLogHandler() {
    setLevel(Level.ALL);
  }

  @Override
  public synchronized void publish(LogRecord lr) {
    logs.add(lr);
  }

  @Override
  public void flush() {}

  @Override
  public void close() throws SecurityException {}

  public void reset() {
    logs.clear();
  }

  public synchronized Object[] assertMessage(String loggerName, String msgPrefix) {
    var f = new SimpleFormatter();
    var sb = new StringBuilder();
    sb.append("Cannot find ").append(msgPrefix).append(" in ").append(loggerName);
    for (var r : logs) {
      if (loggerName.equals(r.getLoggerName()) && r.getMessage().startsWith(msgPrefix)) {
        return r.getParameters();
      }
      sb.append("\n").append(f.format(r));
    }
    fail(sb.toString());
    return null;
  }
}
