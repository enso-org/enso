package org.enso.logger.config;

import com.typesafe.config.Config;

public class ConsoleAppender extends Appender {

  private final String pattern;

  private ConsoleAppender(String pattern, Config config) {
    super(config);
    this.pattern = pattern;
  }

  public static ConsoleAppender parse(Config config) {
    String pattern = config.hasPath("pattern") ? config.getString("pattern") : null;
    return new ConsoleAppender(pattern, config);
  }

  public String getPattern() {
    return pattern;
  }

  @Override
  public String getName() {
    return "console";
  }
}
