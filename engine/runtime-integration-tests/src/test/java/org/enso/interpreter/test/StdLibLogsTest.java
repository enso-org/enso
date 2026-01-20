package org.enso.interpreter.test;

import static org.junit.Assert.*;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.util.List;
import org.enso.common.MethodNames;
import org.enso.logging.service.logback.MemoryAppender;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public class StdLibLogsTest {

  private static Value mod;
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @BeforeClass
  public static void initEnsoContext() {
    mod =
        ctxRule.eval(
            "enso",
            """
            from Standard.Base import IO
            from Standard.Base.Logging import all

            type Foo

            test =
                Foo.log_message level=..Warning "I should warn you about something..."
                Foo.log_message level=..Info "Should be seen? By default we only show up-to warnings level"
                Foo.log_message level=..Severe "Something went really bad!"
            """);
  }

  @AfterClass
  public static void disposeContext() {
    mod = null;
  }

  @Test
  public void reportLogsInStdLib() {
    mod.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "test");
    var context = (LoggerContext) LoggerFactory.getILoggerFactory();
    var logger = context.getLogger(Logger.ROOT_LOGGER_NAME);
    var appender = (MemoryAppender) logger.getAppender("memory");
    var events = appender.getEvents().stream().map(ILoggingEvent::getMessage).toList();

    assertTrue(
        events.containsAll(
            List.of("I should warn you about something...", "Something went really bad!")));
  }
}
