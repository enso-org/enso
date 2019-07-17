package org.enso.interpreter.runtime.errors;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class NotInvokableException extends RuntimeException implements TruffleException {
  private final Node node;

  public NotInvokableException(Object target, Node node) {
    super("Object " + target + " is not invokable.");
    this.node = node;
  }

  @Override
  public Node getLocation() {
    return node;
  }
}
