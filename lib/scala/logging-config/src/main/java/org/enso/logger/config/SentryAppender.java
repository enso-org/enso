package org.enso.logger.config;

import com.typesafe.config.Config;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/** Config for log configuration that sends logs to sentry.io service. */
public class SentryAppender extends Appender {

  public String getDsn() {
    return dsn;
  }

  private String dsn;

  private SentryAppender(String dsn) {
    this.dsn = dsn;
  }

  public static Appender parse(Config config) throws MissingConfigurationField {
    if (config.hasPath(dsnKey)) return new SentryAppender(config.getString(dsnKey));
    else throw new MissingConfigurationField(dsnKey);
  }

  @Override
  public Boolean setup(Level logLevel, LoggerSetup appenderSetup) {
    return appenderSetup.setupSentryAppender(logLevel);
  }

  @Override
  public String getName() {
    return appenderName;
  }

  private static final String dsnKey = "dsn";
  public static final String appenderName = "sentry";
}
