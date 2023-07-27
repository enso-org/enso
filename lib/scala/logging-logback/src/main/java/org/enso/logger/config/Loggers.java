package org.enso.logger.config;

import com.typesafe.config.Config;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Loggers {

  private Map<String, ch.qos.logback.classic.Level> loggers;

  private Loggers(Map<String, ch.qos.logback.classic.Level> loggers) {
    this.loggers = loggers;
  }

  public Set<Map.Entry<String, ch.qos.logback.classic.Level>> entrySet() {
    return loggers.entrySet();
  }

  public static Loggers parse(Config config) {
    // LinkedHashMap ensures that wildcard loggers are de-prioritized
    Map<String, ch.qos.logback.classic.Level> loggers = new LinkedHashMap<>();
    Map<String, ch.qos.logback.classic.Level> fallbacks = new LinkedHashMap<>();
    config
        .entrySet()
        .forEach(
            entry -> {
              String key = entry.getKey();
              String v = config.getString(key);
              ch.qos.logback.classic.Level level = ch.qos.logback.classic.Level.toLevel(v);
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
    return new Loggers(loggers);
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
}
