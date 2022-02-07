package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/** An exception thrown when the program tries to redefine an already-defined method */
public class RedefinedConversionException extends AbstractTruffleException {

  /**
   * Creates a new error.
   *
   * @param atom the name of the atom you are converting to
   * @param source the name of the atom you are converting from
   */
  public RedefinedConversionException(String atom, String source) {
    super("You have already overloaded conversion from " + source + " to " + atom);
  }
}
