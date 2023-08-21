package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import org.slf4j.event.Level;

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

  @Override
  public Boolean setup(Level logLevel, AppenderSetup appenderSetup) {
    return appenderSetup.setupConsoleAppender(logLevel);
  }

  @Override
  public Boolean setupForPath(
      Level logLevel,
      Path componentLogPath,
      String componentLogPrefix,
      AppenderSetup appenderSetup) {
    return appenderSetup.setupConsoleAppender(logLevel);
  }

  @Override
  public Boolean setupForURI(Level logLevel, String host, int port, AppenderSetup appenderSetup) {
    return appenderSetup.setupConsoleAppender(logLevel);
  }

  public String getPattern() {
    return pattern;
  }

  @Override
  public String getName() {
    return "console";
  }
}
