package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/** Config for log configuration that sends logs to sentry.io service. */
public final class SentryAppender extends Appender {

  public String getDsn() {
    return dsn;
  }

  private String dsn;

  public Integer getFlushTimeoutMs() {
    return flushTimeoutMs;
  }

  private Integer flushTimeoutMs;

  public boolean isDebugEnabled() {
    return debugEnabled;
  }

  private boolean debugEnabled;

  private SentryAppender(String dsn, Integer flushTimeoutMs, boolean debugEnabled) {
    this.dsn = dsn;
    this.flushTimeoutMs = flushTimeoutMs;
    this.debugEnabled = debugEnabled;
  }

  public static Appender parse(Config config) throws MissingConfigurationField {
    if (config.hasPath(dsnKey)) {
      String dsn = config.getString(dsnKey);
      Integer flushTimeoutMs =
          config.hasPath(flushTimeoutKey) ? Integer.valueOf(config.getInt(flushTimeoutKey)) : null;
      boolean debugEnabled = config.hasPath(debugKey) ? config.getBoolean(debugKey) : false;
      return new SentryAppender(dsn, flushTimeoutMs, debugEnabled);
    } else throw new MissingConfigurationField(dsnKey);
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return loggerSetup.setupSentryAppender(logLevel, null);
  }

  @Override
  public boolean setupForPath(
      Level logLevel, Path logRoot, String logPrefix, LoggerSetup loggerSetup) {
    if (loggerSetup.getConfig().logToFile()) {
      loggerSetup.setupFileAppender(Level.TRACE, logRoot, logPrefix);
    }
    return loggerSetup.setupSentryAppender(logLevel, logRoot);
  }

  @Override
  public String getName() {
    return appenderName;
  }

  private static final String dsnKey = "dsn";

  private static final String flushTimeoutKey = "flush-timeout";

  private static final String debugKey = "debug";
  public static final String appenderName = "sentry";
}
