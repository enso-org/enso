package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.nodes.RootNode;

/** Annotation with callable expression. */
public class Annotation {

  private final RootNode expression;
  private final String name;

  public Annotation(String name, RootNode expression) {
    this.name = name;
    this.expression = expression;
  }

  /**
   * @return the annotation name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return the annotation expression.
   */
  public RootNode getExpression() {
    return expression;
  }
}
