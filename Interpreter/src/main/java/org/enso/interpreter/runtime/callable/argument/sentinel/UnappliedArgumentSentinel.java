package org.enso.interpreter.runtime.callable.argument.sentinel;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/**
 * A value used to sentinel an argument that has been defined for a function but not yet applied.
 */
public class UnappliedArgumentSentinel {
  private ArgumentDefinition argument;

  /**
   * Creates a sentinel value for the given argument.
   *
   * @param argument information about the argument being signalled
   */
  public UnappliedArgumentSentinel(ArgumentDefinition argument) {
    this.argument = argument;
  }
}
