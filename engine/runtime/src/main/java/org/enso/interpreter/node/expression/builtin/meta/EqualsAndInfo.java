package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.Truffle;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;

/** Holds result of equality check with addtional information about warnings. */
public final class EqualsAndInfo {
  private static final Assumption NO_WARNINGS =
      Truffle.getRuntime().createAssumption("No equals with warnings");

  /** {@code isTrue()} without any warnings */
  static final EqualsAndInfo TRUE = new EqualsAndInfo(true, null);

  /** {@code !isTrue()} value without any warnings */
  static final EqualsAndInfo FALSE = new EqualsAndInfo(false, null);

  private boolean equals;
  private EnsoHashMap warnings;

  private EqualsAndInfo(boolean equals, EnsoHashMap warnings) {
    this.equals = equals;
    this.warnings = warnings;
  }

  /**
   * Checks whether the value of the result is {@code true}. Optimizes the check if no warnings were
   * yet used in the equality check.
   *
   * @return value of {@code result}
   */
  public final boolean isTrue() {
    if (NO_WARNINGS.isValid()) {
      return this == TRUE;
    } else {
      return equals;
    }
  }

  /**
   * Getter for associated warnings.
   *
   * @return {@code null} if there are no warnings, or the warnings
   */
  final EnsoHashMap getWarnings() {
    if (NO_WARNINGS.isValid()) {
      return null;
    } else {
      return warnings;
    }
  }

  /**
   * Value for given result without any warnings.
   *
   * @param b the result
   * @return either {@code TRUE} or {@code FALSE}
   */
  static EqualsAndInfo valueOf(boolean b) {
    return b ? TRUE : FALSE;
  }

  /**
   * Converts {@code result} and {@code warnings} into instance of this class. Speculates on {@code
   * warnings} being {@code null}.
   *
   * @param result result of {@code ==} operation.
   * @param warnings associated warnings or {@code null}
   * @return either {@code TRUE} or {@code FALSE} or instance representing both values
   */
  static EqualsAndInfo valueOf(boolean result, EnsoHashMap warnings) {
    if (warnings == null) {
      return result ? TRUE : FALSE;
    } else {
      NO_WARNINGS.invalidate("Found equals with warnings");
      return new EqualsAndInfo(result, warnings);
    }
  }
}
