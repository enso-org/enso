package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/**
 * Holds the definition site argument information together with information on the partially applied
 * arguments positions.
 */
public class ArgumentSchema {
  private final @CompilationFinal(dimensions = 1) ArgumentDefinition[] argumentInfos;
  private final @CompilationFinal(dimensions = 1) boolean[] hasPreApplied;
  private final @CompilationFinal(dimensions = 1) CallArgumentInfo[] oversaturatedArguments;
  private final boolean hasAnyPreApplied;
  private final boolean hasOversaturatedArguments;

  /**
   * Creates an {@link ArgumentSchema} instance.
   *
   * @param argumentInfos Definition site arguments information
   * @param hasPreApplied A flags collection such that {@code hasPreApplied[i]} is true iff a
   *     function has a partially applied argument at position {@code i}
   */
  public ArgumentSchema(
      ArgumentDefinition[] argumentInfos,
      boolean[] hasPreApplied,
      CallArgumentInfo[] oversaturatedArguments) {
    this.argumentInfos = argumentInfos;
    this.oversaturatedArguments = oversaturatedArguments;
    this.hasPreApplied = hasPreApplied;
    boolean hasAnyPreApplied = false;

    for (boolean b : hasPreApplied) {
      if (b) {
        hasAnyPreApplied = true;
        break;
      }
    }

    this.hasAnyPreApplied = hasAnyPreApplied;
    this.hasOversaturatedArguments = this.oversaturatedArguments.length > 0;
  }

  /**
   * Creates an {@link ArgumentSchema} instance assuming the function has no partially applied
   * arguments.
   *
   * @param argumentInfos Definition site arguments information
   */
  public ArgumentSchema(ArgumentDefinition... argumentInfos) {
    this(argumentInfos, new boolean[argumentInfos.length], new CallArgumentInfo[0]);
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
   * Checks if the schema has associated oversaturated arguments.
   *
   * @return {@code true} if the schema has oversaturated arguments, otherwise {@code false}
   */
  public boolean hasOversaturatedArgs() {
    return this.hasOversaturatedArguments;
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
   * Gets a copy of the oversaturated arguments in the schema.
   *
   * @return a copy of the array containing info on the oversaturated arguments
   */
  public CallArgumentInfo[] cloneOversaturatedArgs() {
    return this.oversaturatedArguments.clone();
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

  /**
   * Returns the oversaturated arguments contained within the schema.
   *
   * @return an array of the oversaturated arguments in the schema
   */
  public CallArgumentInfo[] getOversaturatedArguments() {
    return oversaturatedArguments;
  }
}
