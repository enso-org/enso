package org.enso.logging.config.systemlogger;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;

public class SystemViaSlf4jViaLoggerTest {
  @Test
  public void logAMessage() {
    var mock = new MockSystemLogger("first.logger", Level.INFO);
    var sysLog = new SystemViaSlf4jLogger(mock);

    assertTrue("Error is loggable", sysLog.isLoggable(System.Logger.Level.ERROR));
    sysLog.log(System.Logger.Level.ERROR, "This will be OK!");
    mock.assertLogged(Level.ERROR, "This will be OK!", new Object[0]);

    assertFalse("Debug is not loggable", sysLog.isLoggable(System.Logger.Level.DEBUG));
    sysLog.log(System.Logger.Level.DEBUG, "Not logged at all");
    mock.assertLogged(null, null, null);
  }

  @Test
  public void logMessageWithArguments() {
    var mock = new MockSystemLogger("second.logger", Level.INFO);
    var sysLog = new SystemViaSlf4jLogger(mock);

    assertTrue("Warning is loggable", sysLog.isLoggable(System.Logger.Level.WARNING));
    var jdkFmt = "One {0} two {1} three {2}!";
    var args = new Object[] {1, 2, 3};
    sysLog.log(System.Logger.Level.WARNING, jdkFmt, args);
    var slf4jFmt = "One {} two {} three {}!";
    mock.assertLogged(Level.WARN, slf4jFmt, args);

    var jdkMsg = java.text.MessageFormat.format(jdkFmt, args);
    var slf4Msg = org.slf4j.helpers.MessageFormatter.arrayFormat(slf4jFmt, args);

    assertEquals("Both formatted messages are the same", slf4Msg.getMessage(), jdkMsg);
  }

  @Test
  public void logMessageTypedWithArguments() {
    var mock = new MockSystemLogger("second.logger", Level.INFO);
    var sysLog = new SystemViaSlf4jLogger(mock);

    assertTrue("Warning is loggable", sysLog.isLoggable(System.Logger.Level.WARNING));
    var jdkFmt = "One {0,number} two {1} three {2}!";
    var args = new Object[] {1, "2", 3};
    sysLog.log(System.Logger.Level.WARNING, jdkFmt, args);
    var slf4jFmt = "One {} two {} three {}!";
    mock.assertLogged(Level.WARN, slf4jFmt, args);

    var jdkMsg = java.text.MessageFormat.format(jdkFmt, args);
    var slf4Msg = org.slf4j.helpers.MessageFormatter.arrayFormat(slf4jFmt, args);

    assertEquals("Both formatted messages are the same", slf4Msg.getMessage(), jdkMsg);
  }

  private static final class MockSystemLogger extends AbstractLogger {
    private final Level minLevel;
    private final String name;
    private Level loggedLevel;
    private String loggedFormat;
    private Object[] loggedParams;

    public MockSystemLogger(String name, Level minLevel) {
      this.name = name;
      this.minLevel = minLevel;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    protected String getFullyQualifiedCallerName() {
      return null;
    }

    @Override
    protected void handleNormalizedLoggingCall(
        Level level, Marker marker, String format, Object[] params, Throwable throwable) {
      assertNull("No throwable now", throwable);
      assertNotNull("Logged level cannot be null", level);
      assertNull("No previous level yet", this.loggedLevel);

      this.loggedLevel = level;
      this.loggedFormat = format;
      this.loggedParams = params;
    }

    final void assertLogged(Level expLevel, String expFormat, Object[] expParams) {
      assertEquals("Same level", expLevel, loggedLevel);
      assertEquals("Same format", expFormat, loggedFormat);
      assertArrayEquals("Same params", expParams, loggedParams);

      loggedLevel = null;
      loggedFormat = null;
      loggedParams = null;
    }

    @Override
    public boolean isTraceEnabled() {
      return minLevel.compareTo(Level.TRACE) >= 0;
    }

    @Override
    public boolean isTraceEnabled(Marker marker) {
      return minLevel.compareTo(Level.TRACE) >= 0;
    }

    @Override
    public boolean isDebugEnabled() {
      return minLevel.compareTo(Level.DEBUG) >= 0;
    }

    @Override
    public boolean isDebugEnabled(Marker marker) {
      return minLevel.compareTo(Level.DEBUG) >= 0;
    }

    @Override
    public boolean isInfoEnabled() {
      return minLevel.compareTo(Level.INFO) >= 0;
    }

    @Override
    public boolean isInfoEnabled(Marker marker) {
      return minLevel.compareTo(Level.INFO) >= 0;
    }

    @Override
    public boolean isWarnEnabled() {
      return minLevel.compareTo(Level.WARN) >= 0;
    }

    @Override
    public boolean isWarnEnabled(Marker marker) {
      return minLevel.compareTo(Level.WARN) >= 0;
    }

    @Override
    public boolean isErrorEnabled() {
      return minLevel.compareTo(Level.ERROR) >= 0;
    }

    @Override
    public boolean isErrorEnabled(Marker marker) {
      return minLevel.compareTo(Level.ERROR) >= 0;
    }
  }
}
