package org.enso.base;

import java.util.HashMap;
import java.util.function.Supplier;

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

  /**
   * Overrides the System environment variable with a new value. The override is only visible from
   * within Enso.
   *
   * <p>This is an internal function that should be used very carefully and only for testing.
   */
  public static <T> T with_environment_variable_override(
      String name, String value, Supplier<T> action) {
    String oldValue = overrides.put(name, value);
    boolean was_set = oldValue != null;
    try {
      return action.get();
    } finally {
      if (was_set) {
        overrides.put(name, oldValue);
      } else {
        overrides.remove(name);
      }
    }
  }

  private static final HashMap<String, String> overrides = new HashMap<>();
}
