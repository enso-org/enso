package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItemInArray;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import org.enso.logging.service.logback.MemoryAppender;
import org.enso.test.utils.ContextUtils;
import org.enso.testkit.ReportLogsOnFailureRule;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public class StdLibLoggingTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Rule public ReportLogsOnFailureRule appenderRule = new ReportLogsOnFailureRule();

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
    ctxRule.eval(logExample).invokeMember("eval_expression", "test");
    var events = appender.getEvents().stream().map(ILoggingEvent::getMessage).toArray();

    assertThat(events, hasItemInArray("Logging something"));
  }
}
