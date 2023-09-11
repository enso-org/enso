package org.enso.logger.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.slf4j.event.Level;

/** Encapsulates custom log levels that can be set via config file and environmental variables. */
public class LoggersLevels {

  private Map<String, Level> loggers;

  private LoggersLevels(Map<String, Level> loggers) {
    this.loggers = loggers;
  }

  public Set<Map.Entry<String, Level>> entrySet() {
    return loggers.entrySet();
  }

  public static LoggersLevels parse() {
    return parse(ConfigFactory.empty());
  }

  public static LoggersLevels parse(Config config) {
    // LinkedHashMap ensures that wildcard loggers are de-prioritized
    Map<String, Level> loggers = systemLoggers();
    Map<String, Level> fallbacks = new LinkedHashMap<>();
    config
        .entrySet()
        .forEach(
            entry -> {
              String key = entry.getKey();
              String v = config.getString(key);
              Level level = Level.valueOf(v.toUpperCase());
              String normalizedKey = normalizeKey(key);

              if (normalizedKey.endsWith("*")) {
                int idx = normalizedKey.indexOf('*');
                fallbacks.put(normalizedKey.substring(0, idx), level);
              } else {
                loggers.put(normalizedKey, level);
              }
            });
    if (!fallbacks.isEmpty()) {
      loggers.putAll(fallbacks);
    }
    return new LoggersLevels(loggers);
  }

  public boolean isEmpty() {
    return loggers.isEmpty();
  }

  /**
   * Read any loggers' levels set via `-Dfoo.bar.Logger.level=<level>` env variables.
   *
   * @return a map of custom loggers' levels set on startup
   */
  private static Map<String, Level> systemLoggers() {
    Map<String, Level> loggers = new LinkedHashMap<>();
    System.getProperties()
        .forEach(
            (keyObj, value) -> {
              String key = keyObj.toString();
              if (key.endsWith(SYS_PROP_SUFFIX)) {
                int idx = key.lastIndexOf(SYS_PROP_SUFFIX);
                String loggerName = key.substring(0, idx);
                try {
                  loggers.put(loggerName, Level.valueOf(value.toString().toUpperCase()));
                } catch (IllegalArgumentException e) {
                  System.err.println(
                      "Invalid log level `" + value + "` for " + loggerName + ". Skipping...");
                }
              }
            });
    return loggers;
  }

  @Override
  public java.lang.String toString() {
    return String.join(
        "\n",
        loggers.entrySet().stream()
            .map(entry -> entry.getKey() + ": " + entry.getValue().toString())
            .collect(Collectors.toList()));
  }

  private static String normalizeKey(String key) {
    return key.replace("'", "").replace("\"", "");
  }

  private static String SYS_PROP_SUFFIX = ".Logger.level";
}
