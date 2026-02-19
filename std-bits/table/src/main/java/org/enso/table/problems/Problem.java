package org.enso.table.problems;

import org.graalvm.polyglot.Value;

/**
 * A parent class for problems which may be reported as warnings or errors, depending on the setup.
 */
public interface Problem {
  /** Converts the problem to an Enso value. */
  Value asEnsoValue();

  /** Flag to indicate if this problem is an error. */
  default boolean isError() {
    return false;
  }
}
