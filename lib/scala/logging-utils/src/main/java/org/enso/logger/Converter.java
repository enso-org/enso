package org.enso.logger;

import static org.slf4j.event.Level.DEBUG;
import static org.slf4j.event.Level.ERROR;
import static org.slf4j.event.Level.INFO;
import static org.slf4j.event.Level.TRACE;
import static org.slf4j.event.Level.WARN;

import org.slf4j.event.Level;

public final class Converter {
  private Converter() {}

  /** Determines what is the smallest Java level that is still debug and not trace. */
  private static final int defaultLevelDebugCutOff =
      Math.min(java.util.logging.Level.FINE.intValue(), java.util.logging.Level.CONFIG.intValue());

  /**
   * Converts SLF4J's Level to java.util one.
   *
   * @param level the SLF4J's level to convert.
   * @return an equivalent in java.util.logging.Level terms
   */
  public static java.util.logging.Level toJavaLevel(Level level) {
    return switch (level) {
      case ERROR -> java.util.logging.Level.SEVERE;
      case WARN -> java.util.logging.Level.WARNING;
      case INFO -> java.util.logging.Level.INFO;
      case DEBUG -> java.util.logging.Level.FINE;
      case TRACE -> java.util.logging.Level.FINEST;
      default -> java.util.logging.Level.ALL;
    };
  }

  /** Default mapping of Java log levels to our log levels based */
  public static Level fromJavaLevel(java.util.logging.Level javaLevel) {
    int level = javaLevel.intValue();
    if (level == java.util.logging.Level.OFF.intValue()) return ERROR;
    else if (level >= java.util.logging.Level.SEVERE.intValue()) return ERROR;
    else if (level >= java.util.logging.Level.WARNING.intValue()) return WARN;
    else if (level >= java.util.logging.Level.INFO.intValue()) return INFO;
    else if (level >= defaultLevelDebugCutOff) return DEBUG;
    else return TRACE;
  }
}
