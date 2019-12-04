package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.RootCallTarget;
import org.enso.interpreter.node.ExpressionNode;

/** Tracks the specifics about how arguments are specified at a call site. */
public class CallArgument {
  private final String name;
  private final ExpressionNode expression;

  /**
   * Creates an argument passed positionally.
   *
   * @param expression the value of the argument
   */
  public CallArgument(ExpressionNode expression) {
    this(null, expression);
  }

  /**
   * Creates an argument passed by name.
   *
   * @param name the name of the argument being applied
   * @param expression the value of the argument
   */
  public CallArgument(String name, ExpressionNode expression) {
    this.name = name;
    this.expression = expression;
  }


  /**
   * Checks if the argument is passed by name.
   *
   * @return {@code true} if it is passed by name, otherwise {@code false}
   */
  public boolean isNamed() {
    return this.name != null;
  }

  /**
   * Checks if the argument is passed by position.
   *
   * @return {@code true} if it is passed by position, otherwise {@code false}
   */
  public boolean isPositional() {
    return !isNamed();
  }

  /**
   * Gets the argument name.
   *
   * @return the name of the argument
   */
  public String getName() {
    return this.name;
  }

  /**
   * Gets the expression representing the argument's value.
   *
   * @return the expression representing the value of the argument
   */
  public ExpressionNode getExpression() {
    return expression;
  }
}
