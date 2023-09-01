package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/** Config for log configuration that appends to the file. */
public final class FileAppender extends Appender {
  private final boolean append;
  private final boolean immediateFlush;
  private final String pattern;

  private final LogLocation logLocation;
  private final RollingPolicy rollingPolicy;

  private FileAppender(
      boolean append,
      boolean immediateFlush,
      String pattern,
      LogLocation logLocation,
      RollingPolicy rollingPolicy) {
    this.append = append;
    this.immediateFlush = immediateFlush;
    this.pattern = pattern;

    this.logLocation = logLocation;
    this.rollingPolicy = rollingPolicy;
  }

  public static Appender parse(Config config) {
    boolean append = config.hasPath(appendKey) ? config.getBoolean(appendKey) : true;
    boolean immediateFlush =
        config.hasPath(immediateFlushKey) ? config.getBoolean(immediateFlushKey) : false;
    String pattern =
        config.hasPath(patternKey) ? config.getString(patternKey) : Appender.defaultPattern;

    LogLocation location;
    if (config.hasPath(logLocationKey)
        && config.hasPath(logRootKey)
        && config.hasPath(logPrefixKey)) {
      Config logLocationConfig = config.getConfig(logLocationKey);
      location =
          new LogLocation(
              Paths.get(logLocationConfig.getString(logRootKey)),
              logLocationConfig.getString(logPrefixKey));
    } else {
      location = new LogLocation(null, null);
    }

    RollingPolicy rollingPolicy;
    if (config.hasPath(rollingPolicyKey)) {
      Config c = config.getConfig(rollingPolicyKey);
      rollingPolicy =
          new RollingPolicy(
              stringWithDefault(c, maxFileSizeKey, "50MB"),
              initWithDefault(c, maxHistoryKey, 30),
              stringWithDefault(c, maxTotalSizeKey, "2GB"));
    } else {
      rollingPolicy = null;
    }

    return new FileAppender(append, immediateFlush, pattern, location, rollingPolicy);
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup appenderSetup) {
    return appenderSetup.setupFileAppender(
        logLevel, logLocation.logRoot(), logLocation.logPrefix());
  }

  @Override
  public boolean setupForPath(
      Level logLevel, Path componentLogPath, String componentLogPrefix, LoggerSetup loggerSetup) {
    return loggerSetup.setupFileAppender(logLevel, componentLogPath, componentLogPrefix);
  }

  @Override
  public String getName() {
    return appenderName;
  }

  public boolean isAppend() {
    return append;
  }

  public boolean isImmediateFlush() {
    return immediateFlush;
  }

  public String getPattern() {
    return pattern;
  }

  public RollingPolicy getRollingPolicy() {
    return rollingPolicy;
  }

  public record LogLocation(Path logRoot, String logPrefix) {}

  public record RollingPolicy(String maxFileSize, int maxHistory, String totalSizeCap) {}

  private static int initWithDefault(Config c, String key, int defaultValue) {
    if (c.hasPath(key)) return c.getInt(key);
    else return defaultValue;
  }

  private static String stringWithDefault(Config c, String key, String defaultValue) {
    if (c.hasPath(key)) return c.getString(key);
    else return defaultValue;
  }

  @Override
  public String toString() {
    return "file-appender: pattern - "
        + pattern
        + ", immediate-flush - "
        + immediateFlush
        + ", rolling-policy - "
        + (rollingPolicy == null ? "no" : rollingPolicy.toString());
  }

  // Config keys
  private static final String immediateFlushKey = "immediate-flush";
  private static final String appendKey = "append";
  private static final String patternKey = "pattern";

  private static final String logLocationKey = "location";
  private static final String logRootKey = "log-root";
  private static final String logPrefixKey = "log-prefix";

  private static final String rollingPolicyKey = "rolling-policy";
  private static final String maxFileSizeKey = "max-file-size";
  private static final String maxHistoryKey = "max-history";
  private static final String maxTotalSizeKey = "max-total-size";

  public static final String appenderName = "file";
}
