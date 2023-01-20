package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.nodes.RootNode;

public class Annotation {

  private final RootNode expression;
  private final String name;

  public Annotation(String name, RootNode expression) {
    this.name = name;
    this.expression = expression;
  }

  public String getName() {
    return name;
  }

  public RootNode getExpression() {
    return expression;
  }
}
