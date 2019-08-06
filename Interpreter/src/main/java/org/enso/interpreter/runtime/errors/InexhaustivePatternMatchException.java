package org.enso.interpreter.runtime.errors;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class InexhaustivePatternMatchException extends RuntimeException
    implements TruffleException {

  private final Node node;

  public InexhaustivePatternMatchException(Node node) {
    super("Inexhaustive pattern match.");
    this.node = node;
  }

  @Override
  public Node getLocation() {
    return node;
  }
}
