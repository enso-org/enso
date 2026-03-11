package org.enso.logging.config.systemlogger;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ResourceBundle;
import org.junit.Test;

public class SystemLoggerForSlf4jTest {
  @Test
  public void logAMessage() {
    var mock = new MockSystemLogger("first.logger", System.Logger.Level.INFO);
    var slf4j = new SystemSlf4jLogger(mock);

    assertTrue("Error is loggable", slf4j.isErrorEnabled());
    slf4j.error("This will be OK!");
    mock.assertLogged(System.Logger.Level.ERROR, "This will be OK!", null);

    assertFalse("Debug is not loggable", slf4j.isDebugEnabled());
    slf4j.debug("Not logged at all");
    mock.assertLogged(null, null, null);
  }

  private static final class MockSystemLogger implements System.Logger {
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
    public boolean isLoggable(Level level) {
      return minLevel.compareTo(level) <= 0;
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String msg, Throwable thrown) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String format, Object... params) {
      assertNull("No bundle", bundle);
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
  }
}
