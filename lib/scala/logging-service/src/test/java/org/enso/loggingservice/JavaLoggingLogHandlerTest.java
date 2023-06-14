package org.enso.loggingservice;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import org.enso.loggingservice.internal.InternalLogMessage;
import org.enso.loggingservice.internal.LoggerConnection;
import org.junit.Test;
import scala.collection.immutable.Map;

public class JavaLoggingLogHandlerTest {

  @Test
  public void verifyFormatting() {
    var c =
        new LoggerConnection() {
          final List<InternalLogMessage> messages = new ArrayList<>();

          @Override
          public void send(InternalLogMessage message) {
            messages.add(message);
          }

          @Override
          public LogLevel logLevel() {
            throw new UnsupportedOperationException();
          }

          @Override
          public Map<String, LogLevel> loggers() {
            throw new UnsupportedOperationException();
          }

          @Override
          public boolean isEnabled(String name, LogLevel level) {
            return true;
          }
        };
    var h = new JavaLoggingLogHandler((v1) -> LogLevel.Debug$.MODULE$, c);

    LogRecord r = new LogRecord(Level.SEVERE, "Init {0} done");
    r.setParameters(new Object[] {"js"});

    h.publish(r);

    assertEquals("One message: " + c.messages, 1, c.messages.size());
    assertEquals("Init js done", c.messages.get(0).message());
  }
}
