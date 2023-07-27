package org.enso.logger.akka;

import static org.slf4j.event.Level.*;

import java.util.Optional;
import org.slf4j.event.Level;

public class AkkaConverter {

  /**
   * Converts SLF4K's Level to an Akka one.
   *
   * @param level a SLF4J's Level to convert
   * @return an equivalnet of `level` in Akka's LogLevel
   */
  public static akka.event.Logging.LogLevel toAkka(Level level) {
    switch (level) {
      case ERROR:
        return new akka.event.Logging.LogLevel(1); // Error
      case WARN:
        return new akka.event.Logging.LogLevel(2); // Warning
      case INFO:
        return new akka.event.Logging.LogLevel(3); // Info
      case DEBUG:
        return new akka.event.Logging.LogLevel(4); // Debug
      case TRACE:
        return new akka.event.Logging.LogLevel(4); // Debug
      default:
        return new akka.event.Logging.LogLevel(1);
    }
  }

  /**
   * Converts a string representation of Akka's LogLevel to an SLF4J one.
   *
   * @param level a string representation of level to convert
   * @return an equivalent representation in Akka's LogLevel
   */
  public static Optional<Level> fromString(String level) {
    switch (level.toLowerCase()) {
      case "warning":
        return Optional.of(WARN);
      case "error":
        return Optional.of(ERROR);
      case "info":
        return Optional.of(INFO);
      case "debug":
        return Optional.of(DEBUG);
      case "trace":
        return Optional.of(TRACE);
      default:
        return Optional.empty();
    }
  }
}
