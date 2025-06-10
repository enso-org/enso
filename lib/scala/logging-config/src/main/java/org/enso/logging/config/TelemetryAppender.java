package org.enso.logging.config;

import com.typesafe.config.Config;
import java.net.URI;
import org.slf4j.event.Level;

/** A (so far) empty config for Telemetry appender. */
public final class TelemetryAppender extends Appender {
  public static final String appenderName = "telemetry";
  private final String logsUri;
  private final boolean logConnectionFailures;

  public TelemetryAppender(String apiUri, boolean logConnectionFailures) {
    assert !apiUri.endsWith("/");
    this.logsUri = apiUri + "/logs";
    this.logConnectionFailures = logConnectionFailures;
  }

  public static Appender parse(Config config) throws MissingConfigurationField {
    String uri = config.getString("uri");
    boolean logConnectionFailures = config.getBoolean("log-connection-failures");
    return new TelemetryAppender(uri, logConnectionFailures);
  }

  @Override
  public String getName() {
    return appenderName;
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return loggerSetup.setupTelemetryAppender(URI.create(logsUri), logConnectionFailures);
  }
}
