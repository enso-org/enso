package org.enso.base;

import java.util.HashMap;
import java.util.function.Function;

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
   * Calls `action` with the provided environment variable.
   *
   * <p>The override is not persisted (its only visible from within the action called by this
   * method) and it is only visible by the Enso `Environment.get` method (backed by {@code
   * get_environment_variable}).
   *
   * <p>This is an internal function that should be used very carefully and only for testing.
   */
  public static <T> T with_environment_variable_override(
      String name, String value, Function<Object, T> action) {
    String oldValue = overrides.put(name, value);
    boolean was_set = oldValue != null;
    try {
      // Giving 0 here as an argument, as using null would lead to incorrect behaviour, due to some
      // weird Truffle peculiarity.
      return action.apply(0);
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
