package org.enso.interpreter.runtime.error;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/**
 * An error thrown when an attempt is made to execute a callable that has not had all of its
 * arguments provided.
 */
public class UnsaturatedCallException extends RuntimeException {

  /**
   * Creates the error.
   *
   * @param arg the unapplied argument
   */
  public UnsaturatedCallException(ArgumentDefinition arg) {
    super("The argument named " + arg.getName() + " has not been applied.");
  }
}
