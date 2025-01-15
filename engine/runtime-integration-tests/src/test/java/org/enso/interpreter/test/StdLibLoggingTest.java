package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.util.List;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.logging.service.logback.MemoryAppender;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.*;
import org.slf4j.LoggerFactory;

public class StdLibLoggingTest {
  private Context ctx;
  private static EnsoContext ensoContext;

  @Before
  public void initializeContext() {
    this.ctx = ContextUtils.createDefaultContext();
    this.ensoContext = ContextUtils.leakContext(ctx);
  }

  @After
  public void disposeCtx() {
    ctx.close();
    ctx = null;
    ensoContext.shutdown();
    ensoContext = null;
  }

  private final Source logExample =
      Source.newBuilder(
              "enso",
              """
                  polyglot java import org.enso.example.LoggingTestUtils

                  test =
                      LoggingTestUtils.logSomething
                  """,
              "logs.enso")
          .buildLiteral();

  @Test
  public void testLogInRef() {
    var context = (LoggerContext) LoggerFactory.getILoggerFactory();
    var logger = context.getLogger(Logger.ROOT_LOGGER_NAME);
    var appender = (MemoryAppender) logger.getAppender("memory");
    appender.reset();
    ctx.eval(logExample).invokeMember("eval_expression", "test");
    var events = appender.getEvents().stream().map(ILoggingEvent::getMessage).toList();

    assertEquals(events, List.of("Logging something"));
  }
}
