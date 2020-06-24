package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/**
 * Holds the definition site argument information together with information on the partially applied
 * arguments positions.
 */
public class FunctionSchema {

  /**
   * Denotes the call strategy that should be used whenever a function with this schema is called.
   *
   * <p>For builtin functions, the algorithm for choosing the proper {@link CallStrategy} is as
   * follows: if the node executes user-provided code ({@link
   * org.enso.interpreter.runtime.callable.argument.Thunk} or a {@link Function}) using the Tail
   * Call Optimization machinery (i.e. marking the execution node as tail position), the right
   * choice is {@code DIRECT_WHEN_TAIL}. Otherwise {@code ALWAYS_DIRECT} should be chosen.
   */
  public enum CallStrategy {

    /** Always call the function directly. */
    ALWAYS_DIRECT,
    /** Call the function directly when said function is in tail call position. */
    DIRECT_WHEN_TAIL,
    /** Always call the function using standard tail call machinery. */
    CALL_LOOP;

    /**
     * Should this function be called directly in the given position?
     *
     * @param isTail is this a tail position?
     * @return {@code true} if the function should be called directly, {@code false} otherwise
     */
    public boolean shouldCallDirect(boolean isTail) {
      return this == ALWAYS_DIRECT || (this == DIRECT_WHEN_TAIL && isTail);
    }
  }

  /** Denotes the caller frame access functions with this schema require to run properly. */
  public enum CallerFrameAccess {
    /** Requires full access to the (materialized) caller frame. */
    FULL,
    /** Does not use the caller frame at all. */
    NONE;

    /**
     * Is there any level of caller frame access required by the function?
     *
     * @return {@code true} if the function must be passed the caller frame, {@code false}
     *     otherwise.
     */
    public boolean shouldFrameBePassed() {
      return this != NONE;
    }
  }

  private final @CompilationFinal(dimensions = 1) ArgumentDefinition[] argumentInfos;
  private final @CompilationFinal(dimensions = 1) boolean[] hasPreApplied;
  private final @CompilationFinal(dimensions = 1) CallArgumentInfo[] oversaturatedArguments;
  private final CallStrategy callStrategy;
  private final boolean hasAnyPreApplied;
  private final boolean hasOversaturatedArguments;
  private final CallerFrameAccess callerFrameAccess;

  /**
   * Creates an {@link FunctionSchema} instance.
   *
   * @param callStrategy the call strategy to use for functions having this schema
   * @param callerFrameAccess the declaration of whether access to caller frame is required for this
   *     function
   * @param argumentInfos Definition site arguments information
   * @param hasPreApplied A flags collection such that {@code hasPreApplied[i]} is true iff a
   *     function has a partially applied argument at position {@code i}
   * @param oversaturatedArguments information about any unused, oversaturated arguments passed to
   *     this function so far
   */
  public FunctionSchema(
      CallStrategy callStrategy,
      CallerFrameAccess callerFrameAccess,
      ArgumentDefinition[] argumentInfos,
      boolean[] hasPreApplied,
      CallArgumentInfo[] oversaturatedArguments) {
    this.callStrategy = callStrategy;
    this.argumentInfos = argumentInfos;
    this.oversaturatedArguments = oversaturatedArguments;
    this.hasPreApplied = hasPreApplied;
    this.callerFrameAccess = callerFrameAccess;
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
   * Creates an {@link FunctionSchema} instance assuming the function has no partially applied
   * arguments.
   *
   * @param callStrategy the call strategy to use for this function
   * @param callerFrameAccess the declaration of need to access the caller frame from the function
   * @param argumentInfos Definition site arguments information
   */
  public FunctionSchema(
      CallStrategy callStrategy,
      CallerFrameAccess callerFrameAccess,
      ArgumentDefinition... argumentInfos) {
    this(
        callStrategy,
        callerFrameAccess,
        argumentInfos,
        new boolean[argumentInfos.length],
        new CallArgumentInfo[0]);
  }

  /**
   * Creates an {@link FunctionSchema} instance assuming the function has no partially applied
   * arguments.
   *
   * <p>Caller frame access is assumed to be {@link CallerFrameAccess#NONE}.
   *
   * @param callStrategy the call strategy to use for this function
   * @param argumentInfos Definition site arguments information
   */
  public FunctionSchema(CallStrategy callStrategy, ArgumentDefinition... argumentInfos) {
    this(callStrategy, CallerFrameAccess.NONE, argumentInfos);
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

  /**
   * Returns the call strategy to use for functions with this schema.
   *
   * @return the call strategy to use
   */
  public CallStrategy getCallStrategy() {
    return callStrategy;
  }

  /**
   * Returns the caller frame access declaration for this function.
   *
   * @return the caller frame access declaration
   */
  public CallerFrameAccess getCallerFrameAccess() {
    return callerFrameAccess;
  }

  /**
   * Checks whether the function is already fully applied.
   *
   * @param defaultsExecutionMode should defaulted arguments be taken as applied or not.
   * @return whether or not the function is fully applied.
   */
  @CompilerDirectives.TruffleBoundary
  public boolean isFullyApplied(InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode) {
    boolean functionIsFullyApplied = true;
    for (int i = 0; i < getArgumentsCount(); i++) {
      boolean hasValidDefault = hasDefaultAt(i) && !defaultsExecutionMode.isIgnore();
      boolean hasPreappliedArg = hasPreAppliedAt(i);

      if (!(hasValidDefault || hasPreappliedArg)) {
        functionIsFullyApplied = false;
        break;
      }
    }
    return functionIsFullyApplied;
  }
}
