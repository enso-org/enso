package org.enso.logger;

public final class LoggerUtils {
  private LoggerUtils() {}

  public static String backwardCompatibleName(String name) {
    return switch (name) {
      case "warning" -> "warn";
      default -> name;
    };
  }
}
