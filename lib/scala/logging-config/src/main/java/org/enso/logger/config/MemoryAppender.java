package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/**
 * Configuration for appender that keeps log events in memory and, optionally, forwards them to the
 * underlying appneder.
 */
public final class MemoryAppender extends Appender {

  public static final String appenderName = "memory";

  private final String forwardTo;

  private MemoryAppender(String forwardTo) {
    this.forwardTo = forwardTo;
  }

  public static MemoryAppender parse(Config config) {
    String fowardTo = config.hasPath(forwardToKey) ? config.getString(forwardToKey) : "console";
    return new MemoryAppender(fowardTo);
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup appenderSetup) {
    return appenderSetup.setupMemoryAppender(logLevel);
  }

  @Override
  public boolean setupForPath(
      Level logLevel, Path logRoot, String logPrefix, LoggerSetup loggerSetup) {
    LogToFile logToFileOpt = loggerSetup.getConfig().logToFile();
    if (logToFileOpt.enabled()) {
      Level minLevel =
          Level.intToLevel(Math.min(logToFileOpt.logLevel().toInt(), logLevel.toInt()));
      loggerSetup.setupFileAppender(minLevel, logRoot, logPrefix);
    }
    return loggerSetup.setupMemoryAppender(logLevel);
  }

  public String getTarget() {
    return forwardTo;
  }

  @Override
  public String getName() {
    return appenderName;
  }

  private static final String forwardToKey = "forward-to";
}
