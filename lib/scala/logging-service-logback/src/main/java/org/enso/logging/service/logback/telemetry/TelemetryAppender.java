package org.enso.logging.service.logback.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.util.Arrays;

// TODO: Presunout sem LogJob, apod.

public final class TelemetryAppender extends AppenderBase<ILoggingEvent> {
  private TelemetryAppender() {}

  public static TelemetryAppender create() {
    // TODO: Read ~/.enso/credentials
    // TODO: Read URL endpoint from env vars
    // TODO: No virtual thread executor
    return new TelemetryAppender();
  }

  @Override
  protected void append(ILoggingEvent eventObject) {
    var mdcMap = eventObject.getMDCPropertyMap();
    System.out.printf(
        "TelemetryAppender.append: msg='%s', args=%s, loggerName='%s', threadName='%s', mdcMap={%s}"
            + " %n",
        eventObject.getMessage(),
        Arrays.toString(eventObject.getArgumentArray()),
        eventObject.getLoggerName(),
        eventObject.getThreadName(),
        mdcMap);
  }
}
