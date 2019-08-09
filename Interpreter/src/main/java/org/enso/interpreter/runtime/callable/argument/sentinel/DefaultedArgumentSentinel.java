package org.enso.interpreter.runtime.callable.argument.sentinel;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/** A representation of an argument that has been explicitly defaulted in the program source. */
public class DefaultedArgumentSentinel {
  private ArgumentDefinition argument;

  /**
   * Creates a defaulted argument sentinel value.
   *
   * @param argument information about the defaulted argument
   */
  public DefaultedArgumentSentinel(ArgumentDefinition argument) {
    this.argument = argument;
  }
}
