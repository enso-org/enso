package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

/** An exception thrown when execution attempts to invoke something that cannot be invoked. */
public class NotInvokableException extends AbstractTruffleException {

  /**
   * Creates the error.
   *
   * @param target the construct that was attempted to be invoked
   * @param node the node where the erroneous invocation took place
   */
  public NotInvokableException(Object target, Node node) {
    super("Object " + target + " is not invokable.", node);
  }
}
