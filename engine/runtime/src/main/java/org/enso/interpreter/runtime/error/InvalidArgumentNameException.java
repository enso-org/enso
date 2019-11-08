package org.enso.interpreter.runtime.error;

import java.util.Set;

/**
 * An error thrown when an argument is provided by name, but doesn't match any of the arguments
 * defined on the callable.
 */
public class InvalidArgumentNameException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param name the erroneous argument name at the call site
   * @param availableNames the argument names defined on the callable
   */
  public InvalidArgumentNameException(String name, Set<String> availableNames) {
    super(
        name
            + " is not a valid argument name for a function with arguments: "
            + availableNames.stream().reduce("", (l, r) -> l + ", " + r));
  }
}
