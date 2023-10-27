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
    int lvl;
    switch (level) {
      case ERROR:
        lvl = akka.event.Logging$.MODULE$.ErrorLevel();
        break;
      case WARN:
        lvl = akka.event.Logging$.MODULE$.WarningLevel();
        break;
      case INFO:
        lvl = akka.event.Logging$.MODULE$.InfoLevel();
        break;
      case DEBUG:
        lvl = akka.event.Logging$.MODULE$.DebugLevel();
        break;
      case TRACE:
        lvl = akka.event.Logging$.MODULE$.DebugLevel();
        break;
      default:
        lvl = akka.event.Logging$.MODULE$.ErrorLevel();
    }
    return new akka.event.Logging.LogLevel(lvl);
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
