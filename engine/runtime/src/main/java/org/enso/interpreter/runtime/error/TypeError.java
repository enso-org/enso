package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

/** An exception thrown on a type mismatch. */
public class TypeError extends AbstractTruffleException {

  /**
   * Creates the error.
   *
   * @param message an informative error message
   * @param node the node where the error occurred
   */
  public TypeError(String message, Node node) {
    super(message, node);
  }
}
