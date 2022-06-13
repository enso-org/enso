package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import org.enso.interpreter.runtime.Module;

/** An exception thrown when the program tries to redefine an already-defined constructor */
public class RedefinedConstructorException extends AbstractTruffleException {

  /**
   * Creates a new error.
   *
   * @param atom the method of the atom on which {@code method} is being defined
   */
  public RedefinedConstructorException(String atom, Module scope) {
    super("Constructors cannot be overloaded, but you have tried to overload " + atom + " in scope " + scope.getName());
  }
}
