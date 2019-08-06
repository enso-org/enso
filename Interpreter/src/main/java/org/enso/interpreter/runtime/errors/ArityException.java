package org.enso.interpreter.runtime.errors;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class ArityException extends RuntimeException {
  public ArityException(int expected, int actual) {
    super("Wrong number of arguments. Expected: " + expected + " but got: " + actual + ".");
  }
}
