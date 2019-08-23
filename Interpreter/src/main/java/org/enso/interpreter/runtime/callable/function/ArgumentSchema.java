package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/**
 * Holds the definition site argument information together with information on the partially applied
 * arguments positions.
 */
public class ArgumentSchema {
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) ArgumentDefinition[]
      argumentInfos;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) boolean[] hasPreApplied;
  private final boolean hasAnyPreApplied;

  /**
   * Creates an {@link ArgumentSchema} instance.
   *
   * @param argumentInfos Definition site arguments information
   * @param hasPreApplied A flags collection such that {@code hasPreApplied[i]} is true iff a
   *     function has a partially applied argument at position {@code i}
   */
  public ArgumentSchema(ArgumentDefinition[] argumentInfos, boolean[] hasPreApplied) {
    this.argumentInfos = argumentInfos;
    this.hasPreApplied = hasPreApplied;
    boolean hasAnyPreApplied = false;
    for (int i = 0; i < hasPreApplied.length; i++) {
      if (hasPreApplied[i]) {
        hasAnyPreApplied = true;
        break;
      }
    }
    this.hasAnyPreApplied = hasAnyPreApplied;
  }

  /**
   * Creates an {@link ArgumentSchema} instance assuming the function has no partially applied
   * arguments.
   *
   * @param argumentInfos Definition site arguments information
   */
  public ArgumentSchema(ArgumentDefinition[] argumentInfos) {
    this(argumentInfos, new boolean[argumentInfos.length]);
  }

  /**
   * Does this function have a partially applied argument at a given position?
   *
   * @param i the position for which the check should be performed
   * @return {@code true} if the function had a partially applied argument at this position, {@code
   *     false} otherwise
   */
  public boolean hasPreAppliedAt(int i) {
    return hasPreApplied[i];
  }

  /**
   * Does this function have a default argument at a given position?
   *
   * @param i the position for which the check should be performed
   * @return {@code true} if the function had a default argument at this position, {@code false}
   *     otherwise
   */
  public boolean hasDefaultAt(int i) {
    return argumentInfos[i].hasDefaultValue();
  }

  /**
   * Return the definition site arguments information.
   *
   * @return the definition site arguments information
   */
  public ArgumentDefinition[] getArgumentInfos() {
    return argumentInfos;
  }

  /**
   * Returns a copy of the partial application flags, safe to be mutated by the clients.
   *
   * @return A copy of the partial application flags
   */
  public boolean[] cloneHasPreApplied() {
    return hasPreApplied.clone();
  }

  /**
   * Checks whether this function has any partially applied arguments.
   *
   * @return {@code true} if the function has any partially applied arguments, {@code false}
   *     otherwise
   */
  public boolean hasAnyPreApplied() {
    return hasAnyPreApplied;
  }

  /**
   * Returns the number of arguments defined for this function, including arguments with default
   * values.
   *
   * @return The number of arguments defined for this function
   */
  public int getArgumentsCount() {
    return argumentInfos.length;
  }
}
