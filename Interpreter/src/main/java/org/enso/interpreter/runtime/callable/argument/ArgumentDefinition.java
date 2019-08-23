package org.enso.interpreter.runtime.callable.argument;

import org.enso.interpreter.node.ExpressionNode;

import java.util.Optional;

/** Tracks the specifics about how arguments are defined at the callable definition site. */
public class ArgumentDefinition {
  private final int position;
  private final String name;
  private final Optional<ExpressionNode> defaultValue;

  /**
   * Creates a new argument definition without a default value.
   *
   * @param position the position of the argument at the definition site
   * @param name the name of the argument
   */
  public ArgumentDefinition(int position, String name) {
    this(position, name, null);
  }

  /**
   * Creates a new argument definition with a default value.
   *
   * @param position the position of the argument at the definition site
   * @param name the name of the argument
   * @param defaultValue the default value of the argument
   */
  public ArgumentDefinition(int position, String name, ExpressionNode defaultValue) {
    this.position = position;
    this.name = name;
    this.defaultValue = Optional.ofNullable(defaultValue);
  }

  /**
   * Gets the argument position at the definition site.
   *
   * @return the argument's position at the definition side
   */
  public int getPosition() {
    return this.position;
  }

  /**
   * Gets the argument's name.
   *
   * @return the name of the argument
   */
  public String getName() {
    return this.name;
  }

  /**
   * Gets the argument's default value.
   *
   * @return the default value, if present, otherwise {@link Optional#empty()}
   */
  public Optional<ExpressionNode> getDefaultValue() {
    return this.defaultValue;
  }

  /**
   * Checks if the argument has a default value.
   *
   * @return {@code true} if a default value is present, otherwise {@code false}
   */
  public boolean hasDefaultValue() {
    return this.defaultValue.isPresent();
  }
}
