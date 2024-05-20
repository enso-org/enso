package org.enso.base;

import java.util.HashMap;

public class Environment_Utils {
  /** Gets the environment variable, including any overrides. */
  public static String get_environment_variable(String name) {
    String override = overrides.get(name);
    if (override != null) {
      return override;
    } else {
      return System.getenv(name);
    }
  }

  public static void setOverride(String name, String value) {
    overrides.put(name, value);
  }

  public static void removeOverride(String name) {
    overrides.remove(name);
  }

  public static String getOverride(String name) {
    return overrides.get(name);
  }

  private static final HashMap<String, String> overrides = new HashMap<>();
}
