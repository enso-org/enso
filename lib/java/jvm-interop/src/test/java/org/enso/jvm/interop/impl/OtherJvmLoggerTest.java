package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
    System.setProperty("org.enso.jvm.interop.limit", "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
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
          otherTest.invokeMember("logError", "test.log.error", "I got logged!");
        });

    assertEquals("Logger created", "test.log.error", capture.loggerName);
    assertEquals("Logging at error level maps to severe", Level.SEVERE, capture.loggedLevel);
    assertEquals("The right message", "I got logged!", capture.loggedMsg);
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

  public static void logError(String logName, String msg) {
    var factory = new OtherJvmLogger(CHANNEL);
    var log = factory.getLogger(logName, null);
    log.log(System.Logger.Level.ERROR, msg);
  }
}
