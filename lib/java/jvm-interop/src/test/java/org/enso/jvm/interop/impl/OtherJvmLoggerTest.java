package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmLoggerTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("host") // no dynamic languages needed
          .assertGC(false) // but then we cannot try to GC EnsoContext!
          .build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
    System.setProperty(OtherJvmPool.DUMP_MESSAGE_PROPERTY, "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
            CHANNEL,
            null,
            null,
            (__) -> {
              ctx.context().enter();
              return null;
            },
            (__, ___) -> {
              ctx.context().leave();
            });
  }

  @Test
  public void registerLoggerObtainALog() throws Exception {
    var otherTest = loadOtherJvmClass(OtherJvmLoggerTest.class.getName());

    class CapturingHandler extends Handler {
      String loggerName;
      Level loggedLevel;
      String loggedMsg;

      @Override
      public void publish(LogRecord lr) {
        assertNull("No log record yet", loggerName);
        loggerName = lr.getLoggerName();
        assertNotNull("Logger name set", loggerName);
        loggedLevel = lr.getLevel();
        loggedMsg = lr.getMessage();
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    }
    var capture = new CapturingHandler();
    withLogHandler(
        Logger.getLogger(""),
        capture,
        () -> {
          otherTest.invokeMember("logMessage", "test.log.error", "I got logged!");
        });

    assertEquals("Logger created", "test.log.error", capture.loggerName);
    assertEquals("Logging at error level maps to severe", Level.SEVERE, capture.loggedLevel);
    assertEquals("The right message", "I got logged!", capture.loggedMsg);
  }

  @Test
  public void logWithArguments() throws Exception {
    var otherTest = loadOtherJvmClass(OtherJvmLoggerTest.class.getName());

    class CapturingHandler extends Handler {
      String loggerName;
      Level loggedLevel;
      String loggedMsg;
      Object[] loggedArgs;

      @Override
      public void publish(LogRecord lr) {
        assertNull("No log record yet", loggerName);
        loggerName = lr.getLoggerName();
        assertNotNull("Logger name set", loggerName);
        loggedLevel = lr.getLevel();
        loggedMsg = lr.getMessage();
        loggedArgs = lr.getParameters();
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    }
    var capture = new CapturingHandler();
    withLogHandler(
        Logger.getLogger(""),
        capture,
        () -> {
          otherTest.invokeMember(
              "logWithArguments", "test.log.error", "One {0}, two {1}, when {2}");
        });

    assertEquals("Logger created", "test.log.error", capture.loggerName);
    assertEquals("Logging at error level maps to severe", Level.SEVERE, capture.loggedLevel);
    assertEquals("The right message", "One {0}, two {1}, when {2}", capture.loggedMsg);
    assertEquals("Three args", 3, capture.loggedArgs.length);
    assertEquals(1, capture.loggedArgs[0]);
    assertEquals(2.0, capture.loggedArgs[1]);
    assertEquals(new java.util.Date(43021432432423L).toString(), capture.loggedArgs[2]);
  }

  @Test
  public void registerLoggerObtainALogOnException() throws Exception {
    var otherTest = loadOtherJvmClass(OtherJvmLoggerTest.class.getName());

    class CapturingHandler extends Handler {
      String loggerName;
      Level loggedLevel;
      String loggedMsg;
      Throwable loggedThrown;

      @Override
      public void publish(LogRecord lr) {
        assertNull("No log record yet", loggerName);
        loggerName = lr.getLoggerName();
        assertNotNull("Logger name set", loggerName);
        loggedLevel = lr.getLevel();
        loggedMsg = lr.getMessage();
        loggedThrown = lr.getThrown();
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    }
    var capture = new CapturingHandler();
    withLogHandler(
        Logger.getLogger(""),
        capture,
        () -> {
          otherTest.invokeMember("logException", "test.log.error", "Throwing", "I got thrown!");
        });

    assertEquals("Logger created", "test.log.error", capture.loggerName);
    assertEquals("Logging at error level maps to severe", Level.SEVERE, capture.loggedLevel);
    assertEquals("The right message", "Throwing", capture.loggedMsg);
    assertNotNull("Exception is transfered", capture.loggedThrown);
    assertEquals(
        "Exception has the right message", "I got thrown!", capture.loggedThrown.getMessage());

    FOUND_STACK_ELEMENT:
    {
      for (var elem : capture.loggedThrown.getStackTrace()) {
        if (elem.getClassName().equals(OtherJvmLoggerTest.class.getName())) {
          if (elem.getMethodName().equals("logException")) {
            break FOUND_STACK_ELEMENT;
          }
        }
        if (elem.getClassName().equals("java.lang.System$Logger")) {
          break;
        }
      }
      capture.loggedThrown.printStackTrace();
      fail("Expecting `logException` in the stack but without any reference to System.Logger!");
    }
  }

  @Test
  public void registerLoggerObtainALogOnExceptionWithNoMessage() throws Exception {
    var otherTest = loadOtherJvmClass(OtherJvmLoggerTest.class.getName());

    class CapturingHandler extends Handler {
      String loggerName;
      Level loggedLevel;
      String loggedMsg;
      Throwable loggedThrown;

      @Override
      public void publish(LogRecord lr) {
        assertNull("No log record yet", loggerName);
        loggerName = lr.getLoggerName();
        assertNotNull("Logger name set", loggerName);
        loggedLevel = lr.getLevel();
        loggedMsg = lr.getMessage();
        loggedThrown = lr.getThrown();
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    }
    var capture = new CapturingHandler();
    withLogHandler(
        Logger.getLogger(""),
        capture,
        () -> {
          otherTest.invokeMember("logException", "test.log.error", null, "I got thrown!");
        });

    assertEquals("Logger created", "test.log.error", capture.loggerName);
    assertEquals("Logging at error level maps to severe", Level.SEVERE, capture.loggedLevel);
    assertNull("The right message", capture.loggedMsg);
    assertNotNull("Exception is transfered", capture.loggedThrown);
    assertEquals(
        "Exception has the right message", "I got thrown!", capture.loggedThrown.getMessage());
  }

  private static Value loadOtherJvmClass(String name) throws Exception {
    var msg = new OtherJvmMessage.LoadClass(name);
    var raw = CHANNEL.execute(OtherJvmResult.class, msg).value(null);
    if (raw instanceof OtherJvmObject other) {
      assertTrue(other.assertChannel(CHANNEL));
    }
    var value = ctx.asValue(raw);
    return value;
  }

  private static void withLogHandler(Logger l, Handler h, Runnable r) {
    var previous = l.getHandlers();
    try {
      for (var p : previous) {
        l.removeHandler(p);
      }
      l.addHandler(h);
      r.run();
    } finally {
      l.removeHandler(h);
      for (var p : previous) {
        l.addHandler(p);
      }
    }
  }

  public static void logMessage(String logName, String msg) {
    var factory = new OtherJvmLogger(CHANNEL);
    var log = factory.getLogger(logName, null);
    log.log(System.Logger.Level.ERROR, msg);
  }

  public static void logWithArguments(String logName, String format) {
    var factory = new OtherJvmLogger(CHANNEL);
    var log = factory.getLogger(logName, null);
    log.log(System.Logger.Level.ERROR, format, 1, 2.0, new java.util.Date(43021432432423L));
  }

  public static void logException(String logName, String msg, String ex) {
    var factory = new OtherJvmLogger(CHANNEL);
    var log = factory.getLogger(logName, null);
    log.log(System.Logger.Level.ERROR, msg, new IllegalStateException(ex));
  }
}
