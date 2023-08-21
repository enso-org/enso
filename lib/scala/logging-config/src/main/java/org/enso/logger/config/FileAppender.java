package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import org.slf4j.event.Level;

public class FileAppender extends Appender {

  private static String immediateFlushKey = "immediate-flush";
  private static String appendKey = "append";

  private static String patternKey = "pattern";
  private final String name;
  private final boolean append;
  private final boolean immediateFlush;
  private final String pattern;

  private FileAppender(boolean append, boolean immediateFlush, String pattern, Config config) {
    super(config);
    this.name = "file";
    this.append = append;
    this.immediateFlush = immediateFlush;
    this.pattern = pattern;
  }

  public static Appender parse(Config config) {
    boolean append = config.hasPath(appendKey) ? config.getBoolean(appendKey) : true;
    boolean immediateFlush =
        config.hasPath(immediateFlushKey) ? config.getBoolean("immediate-flush") : false;
    String pattern = config.hasPath(patternKey) ? config.getString(patternKey) : null;
    return new FileAppender(append, immediateFlush, pattern, config);
  }

  @Override
  public Boolean setup(Level logLevel, AppenderSetup appenderSetup) {
    return appenderSetup.setupFileAppender(logLevel, null, null);
  }

  @Override
  public Boolean setupForPath(
      Level logLevel,
      Path componentLogPath,
      String componentLogPrefix,
      AppenderSetup appenderSetup) {
    return appenderSetup.setupFileAppender(logLevel, componentLogPath, componentLogPrefix);
  }

  @Override
  public String getName() {
    return name;
  }

  public String getPattern() {
    return pattern;
  }
}
