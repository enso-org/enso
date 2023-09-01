package org.enso.logger;

import static org.slf4j.event.Level.*;

import org.slf4j.event.Level;

public class Converter {

  /** Determines what is the smallest Java level that is still debug and not trace. */
  private static int defaultLevelDebugCutOff =
      Math.min(java.util.logging.Level.FINE.intValue(), java.util.logging.Level.CONFIG.intValue());

  /**
   * Converts SLF4J's Level to java.util one.
   *
   * @param level the SLF4J's level to convert.
   * @return an equivalent in java.util.logging.Level terms
   */
  public static java.util.logging.Level toJavaLevel(Level level) {
    switch (level) {
      case ERROR:
        return java.util.logging.Level.SEVERE;
      case WARN:
        return java.util.logging.Level.WARNING;
      case INFO:
        return java.util.logging.Level.INFO;
      case DEBUG:
        return java.util.logging.Level.FINE;
      case TRACE:
        return java.util.logging.Level.FINEST;
      default:
        return java.util.logging.Level.ALL;
    }
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
