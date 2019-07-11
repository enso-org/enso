package org.enso.interpreter.builder;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class VariableDoesNotExistException extends RuntimeException
      implements TruffleException {
    public VariableDoesNotExistException(String name) {
      super("Variable " + name + " is not defined.");
    }

    @Override
    public Node getLocation() {
      return null;
    }
  }
