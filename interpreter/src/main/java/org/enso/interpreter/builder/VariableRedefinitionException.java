package org.enso.interpreter.builder;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class VariableRedefinitionException extends RuntimeException
      implements TruffleException {
    public VariableRedefinitionException(String name) {
      super("Variable " + name + " was already defined in this scope.");
    }

    @Override
    public Node getLocation() {
      return null;
    }
  }
