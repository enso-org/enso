package org.enso.runner;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.logger.Converter;
import org.enso.logger.LevelAndNameFilter;
import org.enso.logger.config.LoggersLevels;

public class LogLevelFilter implements LevelAndNameFilter {
  private final Set<Map.Entry<String, Level>> loggers;
  private final Level defaultLevel;

  public LogLevelFilter(LoggersLevels loggers, Level defaultLevel) {
    this.loggers =
        loggers.entrySet().stream()
            .map(
                e -> new AbstractMap.SimpleEntry<>(e.getKey(), Converter.toJavaLevel(e.getValue())))
            .collect(Collectors.toSet());
    this.defaultLevel = defaultLevel;
  }

  @Override
  public boolean isLoggable(Level level, String name) {

    for (var entry : loggers) {
      if (name.startsWith(entry.getKey())) {
        Level loggerLevel = entry.getValue();
        if (level.intValue() < loggerLevel.intValue()) {
          return false;
        } else {
          return true;
        }
      }
    }
    return level.intValue() >= defaultLevel.intValue();
  }
}
