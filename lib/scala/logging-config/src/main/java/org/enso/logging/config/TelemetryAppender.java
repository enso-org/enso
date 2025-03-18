package org.enso.logging.config;

import com.typesafe.config.Config;
import org.slf4j.event.Level;

/** A (so far) empty config for Telemetry appender. */
public final class TelemetryAppender extends Appender {
  public static final String appenderName = "telemetry";

  public static Appender parse(Config config) throws MissingConfigurationField {
    return new TelemetryAppender();
  }

  @Override
  public String getName() {
    return appenderName;
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return loggerSetup.setupTelemetryAppender();
  }
}
