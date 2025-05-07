package org.enso.logging.config;

import com.typesafe.config.Config;
import java.net.URI;
import org.slf4j.event.Level;

public final class OpenSearchAppender extends Appender {
  public static final String appenderName = "opensearch";
  private final String logsUri;
  private final boolean logConnectionFailures;
  private final boolean enabled;

  public OpenSearchAppender(String apiUri, boolean logConnectionFailures, boolean enabled) {
    this.logsUri = apiUri + "/logs";
    this.logConnectionFailures = logConnectionFailures;
    this.enabled = enabled;
  }

  public static Appender parse(Config config) throws MissingConfigurationField {
    String uri = config.getString("uri");
    boolean logConnectionFailures = config.getBoolean("log-connection-failures");
    boolean enabled = config.getBoolean("enabled");
    return new OpenSearchAppender(uri, logConnectionFailures, enabled);
  }

  @Override
  public String getName() {
    return appenderName;
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return enabled
        && loggerSetup.setupOpenSearchAppender(
            logLevel, URI.create(logsUri), logConnectionFailures);
  }
}
