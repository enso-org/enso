package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class TypeError extends RuntimeException implements TruffleException {

  private final Node node;

  public TypeError(String message, Node node) {
    super(message);
    this.node = node;
  }

  @Override
  public Node getLocation() {
    return node;
  }
}
