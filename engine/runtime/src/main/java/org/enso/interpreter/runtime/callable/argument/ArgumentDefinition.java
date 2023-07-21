package org.enso.interpreter.runtime.callable.argument;

import java.util.Optional;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.Type;

/** Tracks the specifics about how arguments are defined at the callable definition site. */
public final class ArgumentDefinition {

  /** Represents the mode of passing this argument to the function. */
  public enum ExecutionMode {
    /** Argument should be passed fully executed. */
    EXECUTE,
    /** Argument should be passed suspended, as a thunk. */
    PASS_THUNK;

    /**
     * Does the mode indicate argument should be passed suspended?
     *
     * @return {@code true} if the argument should be passed suspended, {@code false} otherwise.
     */
    public boolean isSuspended() {
      return this == PASS_THUNK;
    }
  }

  private final int position;
  private final String name;
  private final Type[] checkType;
  private final ExpressionNode defaultValue;
  private final boolean isSuspended;

  /**
   * Creates a new argument definition with a default value.
   *
   * @param position the position of the argument at the definition site
   * @param name the name of the argument
   * @param checkType {@code null} or type the argument must match
   * @param defaultValue the default value of the argument or {@code null}
   * @param executionMode the execution mode for this argument
   */
  public ArgumentDefinition(
      int position,
      String name,
      Type[] checkType,
      ExpressionNode defaultValue,
      ExecutionMode executionMode) {
    this.position = position;
    this.name = name;
    this.checkType = checkType;
    this.defaultValue = defaultValue;
    this.isSuspended = executionMode.isSuspended();
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
    return Optional.ofNullable(this.defaultValue);
  }

  /**
   * Checks if the argument has a default value.
   *
   * @return {@code true} if a default value is present, otherwise {@code false}
   */
  public boolean hasDefaultValue() {
    return this.defaultValue != null;
  }

  /**
   * Whether this argument is passed lazy or executed on the call-site.
   *
   * @return {@code true} if the argument is lazy, {@code false} otherwise.
   */
  public boolean isSuspended() {
    return isSuspended;
  }

  /**
   * Types to check argument for. When the argument is ascribed with a type, this method returns
   * non-{@code null}.
   *
   * @return {@code null} or list of types to check argument against
   */
  public Type[] getCheckType() {
    return checkType;
  }
}
