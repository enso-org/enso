package org.enso.logger.config;

import com.typesafe.config.Config;

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
  public String getName() {
    return name;
  }
}
