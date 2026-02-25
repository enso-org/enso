package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.ILoggerFactory;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;

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
    var otherLogger = loadOtherJvmClass(OtherJvmLogger.class.getName());
    var createdLoggerName = new String[1];
    var loggedLevel = new Level[1];
    var loggedMsg = new String[1];
    var mockFactory =
        (ILoggerFactory)
            (n) -> {
              Assert.assertNull("No logger name assigned yet", createdLoggerName[0]);
              createdLoggerName[0] = n;
              return new AbstractLogger() {
                @Override
                protected String getFullyQualifiedCallerName() {
                  throw new AssertionError();
                }

                @Override
                protected void handleNormalizedLoggingCall(
                    Level level,
                    Marker marker,
                    String msg,
                    Object[] arguments,
                    Throwable throwable) {
                  assertNull("No level yet", loggedLevel[0]);
                  assertNotNull("Level provided", level);
                  assertNotNull("Some message provided", msg);
                  assertNull("No arguments", arguments);
                  assertNull("No throwable", throwable);
                  assertNull("No marker", marker);

                  loggedLevel[0] = level;
                  loggedMsg[0] = msg;
                }

                @Override
                public boolean isTraceEnabled() {
                  throw new AssertionError();
                }

                @Override
                public boolean isTraceEnabled(Marker marker) {
                  throw new AssertionError();
                }

                @Override
                public boolean isDebugEnabled() {
                  throw new AssertionError();
                }

                @Override
                public boolean isDebugEnabled(Marker marker) {
                  throw new AssertionError();
                }

                @Override
                public boolean isInfoEnabled() {
                  throw new AssertionError();
                }

                @Override
                public boolean isInfoEnabled(Marker marker) {
                  throw new AssertionError();
                }

                @Override
                public boolean isWarnEnabled() {
                  throw new AssertionError();
                }

                @Override
                public boolean isWarnEnabled(Marker marker) {
                  throw new AssertionError();
                }

                @Override
                public boolean isErrorEnabled() {
                  return true;
                }

                @Override
                public boolean isErrorEnabled(Marker marker) {
                  throw new AssertionError();
                }
              };
            };
    otherLogger.invokeMember("registerLoggerFactory", mockFactory);

    var otherTest = loadOtherJvmClass(OtherJvmLoggerTest.class.getName());
    otherTest.invokeMember("logError", "test.log.error", "I got logged!");

    assertEquals("Logger created", "test.log.error", createdLoggerName[0]);
    assertEquals("Logging at error level", Level.ERROR, loggedLevel[0]);
    assertEquals("The right message", "I got logged!", loggedMsg[0]);
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

  public static void logError(String logName, String msg) {
    var factory = new OtherJvmLogger().getLoggerFactory();
    var log = factory.getLogger(logName);
    log.error(msg);
  }
}
