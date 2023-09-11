package org.enso.logger;

public class LoggerUtils {
  public static String backwardCompatibleName(String name) {
    switch (name) {
      case "warning":
        return "warn";
      default:
        return name;
    }
  }
}
