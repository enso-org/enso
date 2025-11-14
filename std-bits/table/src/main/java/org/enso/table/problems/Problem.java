package org.enso.table.problems;

/**
 * A parent class for problems which may be reported as warnings or errors, depending on the setup.
 */
public interface Problem {
  /**
   * Computes simple string description of the problem. This helps to classify the problems
   * regardless of whatever (dual) JVM they come from. Problems of the same type from different JVMs
   * may have different class, but will have the same name.
   *
   * @return by default returns {@code getClass().getSimpleName()}
   */
  default String problemType() {
    return getClass().getSimpleName();
  }
}
