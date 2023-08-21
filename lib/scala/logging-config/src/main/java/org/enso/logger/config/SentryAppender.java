package org.enso.logger.config;

import com.typesafe.config.Config;
import org.slf4j.event.Level;

public class SentryAppender extends Appender {
  private String name;
  private String dsn;

  private SentryAppender(String dsn, Config config) {
    super(config);
    this.name = "sentry";
    this.dsn = dsn;
  }

  public static Appender parse(Config config) {
    return new SentryAppender(config.getString("dsn"), config);
  }

  @Override
  public Boolean setup(Level logLevel, AppenderSetup appenderSetup) {
    return appenderSetup.setupSentryAppender(logLevel, dsn);
  }

  @Override
  public String getName() {
    return name;
  }
}
