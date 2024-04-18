package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import java.util.Arrays;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.Annotation;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/**
 * Holds the definition site argument information together with information on the partially applied
 * arguments positions.
 */
public final class FunctionSchema {
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

  public static final FunctionSchema THUNK =
      FunctionSchema.newBuilder().callerFrameAccess(CallerFrameAccess.NONE).build();

  private final @CompilationFinal(dimensions = 1) ArgumentDefinition[] argumentInfos;
  private final @CompilationFinal(dimensions = 1) boolean[] hasPreApplied;
  private final @CompilationFinal(dimensions = 1) CallArgumentInfo[] oversaturatedArguments;
  private final @CompilationFinal(dimensions = 1) Annotation[] annotations;
  private final boolean hasAnyPreApplied;
  private final boolean hasOversaturatedArguments;
  private final CallerFrameAccess callerFrameAccess;
  private final boolean isFullyApplied;
  private final boolean isProjectPrivate;

  public static Builder newBuilder() {
    return new Builder();
  }

  /**
   * Creates an {@link FunctionSchema} instance.
   *
   * @param callerFrameAccess the declaration of whether access to caller frame is required for this
   *     function.
   * @param argumentInfos Definition site arguments information.
   * @param hasPreApplied A flags collection such that {@code hasPreApplied[i]} is true iff a
   *     function has a partially applied argument at position {@code i}.
   * @param oversaturatedArguments information about any unused, oversaturated arguments passed to
   *     this function so far.
   * @param annotations the list of annotations defined on this function.
   */
  private FunctionSchema(
      CallerFrameAccess callerFrameAccess,
      ArgumentDefinition[] argumentInfos,
      boolean[] hasPreApplied,
      CallArgumentInfo[] oversaturatedArguments,
      Annotation[] annotations,
      boolean isProjectPrivate) {
    this.argumentInfos = argumentInfos;
    this.oversaturatedArguments = oversaturatedArguments;
    this.hasPreApplied = hasPreApplied;
    this.callerFrameAccess = callerFrameAccess;
    this.annotations = annotations;
    this.isProjectPrivate = isProjectPrivate;
    boolean hasAnyPreApplied = false;
    for (boolean b : hasPreApplied) {
      if (b) {
        hasAnyPreApplied = true;
        break;
      }
    }

    this.hasAnyPreApplied = hasAnyPreApplied;
    this.hasOversaturatedArguments = this.oversaturatedArguments.length > 0;
    this.isFullyApplied = isFullyApplied(InvokeCallableNode.DefaultsExecutionMode.EXECUTE);
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

  public boolean isProjectPrivate() {
    return isProjectPrivate;
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
   * Returns the caller frame access declaration for this function.
   *
   * @return the caller frame access declaration
   */
  public CallerFrameAccess getCallerFrameAccess() {
    return callerFrameAccess;
  }

  /**
   * @return annotations defined on this function.
   */
  public Annotation[] getAnnotations() {
    return annotations;
  }

  /**
   * Finds annotation by name.
   *
   * @param name the annotation name.
   * @return the matching annotation expression.
   */
  @CompilerDirectives.TruffleBoundary
  public Annotation getAnnotation(String name) {
    return Arrays.stream(annotations)
        .filter(annotation -> annotation.getName().equals(name))
        .findFirst()
        .orElse(null);
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

  public boolean isFullyApplied() {
    return isFullyApplied;
  }

  public static final class Builder {
    private boolean isProjectPrivate = false;
    private Annotation[] annotations = new Annotation[] {};
    private ArgumentDefinition[] argDefs = new ArgumentDefinition[] {};
    private CallArgumentInfo[] callArgInfos = new CallArgumentInfo[] {};
    private CallerFrameAccess callerFrameAccess = CallerFrameAccess.NONE;
    private boolean[] hasPreapplied = new boolean[] {};

    private Builder() {}

    /** Set the function to be project private. */
    public Builder projectPrivate() {
      this.isProjectPrivate = true;
      return this;
    }

    /**
     * @param annotations the list of annotations defined on this function.
     */
    public Builder annotations(Annotation... annotations) {
      this.annotations = annotations;
      return this;
    }

    /**
     * @param argDefs Definition site arguments information.
     */
    public Builder argumentDefinitions(ArgumentDefinition... argDefs) {
      if (hasPreapplied.length > argDefs.length) {
        throw new AssertionError("hasPreapplied must not be specified before argument definitions");
      }
      this.argDefs = argDefs;
      this.hasPreapplied = new boolean[argDefs.length];
      return this;
    }

    /**
     * @param callArgumentInfos information about any unused, oversaturated arguments passed to this
     *     function so far.
     */
    public Builder oversaturatedArguments(CallArgumentInfo... callArgumentInfos) {
      this.callArgInfos = callArgumentInfos;
      return this;
    }

    /**
     * @param callerFrameAccess the declaration of whether access to caller frame is required for
     *     this function.
     */
    public Builder callerFrameAccess(CallerFrameAccess callerFrameAccess) {
      this.callerFrameAccess = callerFrameAccess;
      return this;
    }

    /**
     * @param hasPreapplied A flags collection such that {@code hasPreApplied[i]} is true iff a
     *     function has a partially applied argument at position {@code i}.
     */
    public Builder hasPreapplied(boolean... hasPreapplied) {
      if (hasPreapplied.length != argDefs.length) {
        throw new AssertionError("Argument definitions must be specified before hasPreapplied");
      }
      this.hasPreapplied = hasPreapplied;
      return this;
    }

    public FunctionSchema build() {
      assert argDefs.length == hasPreapplied.length;
      return new FunctionSchema(
          callerFrameAccess, argDefs, hasPreapplied, callArgInfos, annotations, isProjectPrivate);
    }
  }
}
