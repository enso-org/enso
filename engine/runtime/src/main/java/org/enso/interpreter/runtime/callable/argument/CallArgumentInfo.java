package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import java.util.OptionalInt;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;

/**
 * Tracks simple information about call-site arguments, used to make processing of caller argument
 * lists much more simple.
 */
public final class CallArgumentInfo {
  private final String name;

  /**
   * Creates the information from a {@link CallArgument}.
   *
   * @param callArgNode the structure to take information from
   */
  public CallArgumentInfo(CallArgument callArgNode) {
    this(callArgNode.getName());
  }

  /**
   * Creates a named call argument.
   *
   * @param name the name of the argument, if present
   */
  public CallArgumentInfo(String name) {
    this.name = name;
  }

  /** Creates an unnamed call argument. */
  public CallArgumentInfo() {
    this.name = null;
  }

  /**
   * Gets the name of the argument.
   *
   * @return the name of the argument at the call site, if specified
   */
  public String getName() {
    return name;
  }

  /**
   * Checks whether the argument was applied by name or not.
   *
   * @return {@code true} if the argument was applied by name, otherwise {@code false}
   */
  public boolean isNamed() {
    return name != null;
  }

  /**
   * Checks whether the argument was applied by position or not.
   *
   * @return {@code true} if the argument was applied positionally, otherwise {@code false}
   */
  public boolean isPositional() {
    return !isNamed();
  }

  /**
   * Represents a mapping between the defined arguments of a function and the call site arguments.
   */
  public static class ArgumentMappingBuilder {
    private final int[] appliedMapping;
    private final int[] oversaturatedArgumentMapping;
    private final boolean[] argumentShouldExecute;
    private final ArgumentDefinition[] definitions;
    private final CallArgumentInfo[] callArgs;
    private final CallArgumentInfo[] existingOversaturatedArgs;
    private final boolean[] argumentUsed;
    private final boolean[] callSiteArgApplied;
    private final FunctionSchema originalSchema;
    private int oversaturatedWritePosition = 0;

    /**
     * Creates an unitialised object of this class. This instance is not safe for external use and
     * thus the constructor is private.
     *
     * @param schema the definition site arguments schema
     * @param callArgs the call site arguments schema
     */
    private ArgumentMappingBuilder(FunctionSchema schema, CallArgumentInfo[] callArgs) {
      this.appliedMapping = new int[callArgs.length];
      this.oversaturatedArgumentMapping = new int[callArgs.length];
      this.callSiteArgApplied = new boolean[callArgs.length];
      this.argumentShouldExecute = new boolean[callArgs.length];

      this.callArgs = callArgs;
      this.definitions = schema.getArgumentInfos();
      this.argumentUsed = schema.cloneHasPreApplied();
      this.existingOversaturatedArgs = schema.cloneOversaturatedArgs();
      this.originalSchema = schema;
    }

    /**
     * Generates a full mapping between the definition site arguments and call site arguments.
     *
     * @param schema the definition site arguments schema
     * @param callArgs the call site arguments schema
     * @return the generated argument mapping
     */
    @CompilerDirectives.TruffleBoundary
    public static ArgumentMapping generate(FunctionSchema schema, CallArgumentInfo[] callArgs) {
      ArgumentMappingBuilder mapping = new ArgumentMappingBuilder(schema, callArgs);
      mapping.processArguments();
      return mapping.getAppliedMapping();
    }

    /**
     * Processes all the call site arguments one by one, finding a proper place for them in the
     * generated mapping.
     */
    private void processArguments() {
      for (int i = 0; i < callArgs.length; i++) {
        processArgument(i);
      }
    }

    /**
     * Processes a single call site argument, finding a place for it and inserting it into the
     * generated mapping.
     *
     * @param callArgIndex the index of call site argument to process
     */
    private void processArgument(int callArgIndex) {
      CallArgumentInfo callArg = callArgs[callArgIndex];
      OptionalInt maybePosition;

      if (callArg.isPositional()) {
        maybePosition = findFirstUnusedArgumentBy(definedArg -> true);
      } else {
        maybePosition =
            findFirstUnusedArgumentBy(definedArg -> callArg.getName().equals(definedArg.getName()));
      }

      if (maybePosition.isPresent()) {
        int position = maybePosition.getAsInt();
        appliedMapping[callArgIndex] = position;
        argumentUsed[position] = true;
        callSiteArgApplied[callArgIndex] = true;
        if (!definitions[position].isSuspended()) {
          argumentShouldExecute[callArgIndex] = true;
        }
      } else {
        oversaturatedArgumentMapping[callArgIndex] = oversaturatedWritePosition;
        oversaturatedWritePosition++;
      }
    }

    /**
     * Finds the index of the first not yet applied definition site argument satisfying the given
     * predicate.
     *
     * @param predicate the predicate to match candidate arguments against
     * @return {@code OptionalInt.of(foundIndex)} if the index was found, {@code
     *     OptionalInt.empty()} otherwise.
     */
    private OptionalInt findFirstUnusedArgumentBy(Predicate<ArgumentDefinition> predicate) {
      for (int i = 0; i < definitions.length; i++) {
        boolean argumentCanBeUsed = !argumentUsed[i] && predicate.test(definitions[i]);
        if (argumentCanBeUsed) {
          return OptionalInt.of(i);
        }
      }
      return OptionalInt.empty();
    }

    /**
     * Returns the argument mapping as an array {@code mapping} such that {@code mapping[i]} is the
     * index of i-th call site argument inside the definition site schema.
     *
     * @return the computed argument mapping
     */
    public ArgumentMapping getAppliedMapping() {
      return new ArgumentMapping(
          appliedMapping,
          oversaturatedArgumentMapping,
          callSiteArgApplied,
          argumentShouldExecute,
          getPostApplicationSchema());
    }

    /**
     * Returns an {@link FunctionSchema} object resulting from filling in all the call site
     * arguments in the original definition site schema.
     *
     * @return the post-application arguments schema
     */
    public FunctionSchema getPostApplicationSchema() {
      CallArgumentInfo[] newOversaturatedArgInfo =
          IntStream.range(0, this.callArgs.length)
              .filter(i -> !this.callSiteArgApplied[i])
              .mapToObj(i -> this.callArgs[i])
              .toArray(CallArgumentInfo[]::new);

      CallArgumentInfo[] oversaturatedArgInfo =
          new CallArgumentInfo
              [this.existingOversaturatedArgs.length + newOversaturatedArgInfo.length];

      System.arraycopy(
          this.existingOversaturatedArgs,
          0,
          oversaturatedArgInfo,
          0,
          this.existingOversaturatedArgs.length);
      System.arraycopy(
          newOversaturatedArgInfo,
          0,
          oversaturatedArgInfo,
          this.existingOversaturatedArgs.length,
          newOversaturatedArgInfo.length);

      return new FunctionSchema(
          originalSchema.getCallerFrameAccess(),
          definitions,
          argumentUsed,
          oversaturatedArgInfo,
          originalSchema.getAnnotations());
    }
  }

  /**
   * A class that represents the partitioned mapping of the arguments applied to a given callable.
   */
  public static class ArgumentMapping {
    private final @CompilationFinal(dimensions = 1) int[] appliedArgumentMapping;
    private final @CompilationFinal(dimensions = 1) int[] oversaturatedArgumentMapping;
    private final @CompilationFinal(dimensions = 1) boolean[] isValidAppliedArg;
    private final @CompilationFinal(dimensions = 1) boolean[] argumentShouldExecute;
    private final FunctionSchema postApplicationSchema;

    /**
     * Creates a new instance to represent a mapping.
     *
     * @param appliedArgumentMapping the mapping for the arguments that can actually be applied to
     *     the callable in question
     * @param oversaturatedArgumentMapping the mapping representing any oversaturated arguments to
     *     the callable in question
     * @param isAppliedFlags an array of flags that determines which arguments have been applied to
     *     the callable
     * @param postApplicationSchema the schema resulting from applying this mapping
     */
    private ArgumentMapping(
        int[] appliedArgumentMapping,
        int[] oversaturatedArgumentMapping,
        boolean[] isAppliedFlags,
        boolean[] argumentShouldExecute,
        FunctionSchema postApplicationSchema) {
      this.appliedArgumentMapping = appliedArgumentMapping;
      this.oversaturatedArgumentMapping = oversaturatedArgumentMapping;
      this.isValidAppliedArg = isAppliedFlags;
      this.argumentShouldExecute = argumentShouldExecute;
      this.postApplicationSchema = postApplicationSchema;
    }

    /**
     * Reorders the arguments from the call-site to match the order at the definition site.
     *
     * <p>If an argument is not applied in {@code argValues}, the resultant array will contain
     * {@code null} in any places where an argument was not applied. This is then handled later at
     * the point of reading the arguments, where {@link
     * org.enso.interpreter.node.callable.argument.ReadArgumentNode} will use the default value for
     * that argument.
     *
     * @param argValues the function arguments to reorder, ordered as at the call site
     * @param result the array in which the reordered arguments should be stored
     */
    @ExplodeLoop
    public void reorderAppliedArguments(Object[] argValues, Object[] result) {
      for (int i = 0; i < appliedArgumentMapping.length; i++) {
        if (isValidAppliedArg[i]) {
          result[this.appliedArgumentMapping[i]] = argValues[i];
        }
      }
    }

    /**
     * Gets an array containing the oversaturated arguments represented by this mapping.
     *
     * @param argValues the values of all arguments applied to the callable in question
     * @param result the destination array into which this method will put solely the oversaturated
     *     arguments
     */
    @ExplodeLoop
    public void obtainOversaturatedArguments(Object[] argValues, Object[] result, int offset) {
      for (int i = 0; i < argValues.length; i++) {
        if (!isValidAppliedArg[i]) {
          result[offset + this.oversaturatedArgumentMapping[i]] = argValues[i];
        }
      }
    }

    /**
     * Returns a boolean array where the i-th entry is {@code true} iff the i-th argument should be
     * executed immediately and not passed suspended.
     *
     * @return a boolean array where the i-th entry is {@code true} iff the i-th argument should be
     *     executed immediately and not passed suspended.
     */
    public boolean[] getArgumentShouldExecute() {
      return argumentShouldExecute;
    }

    /**
     * Returns the function schema resulting from applying this mapping.
     *
     * @return the post application schema
     */
    public FunctionSchema getPostApplicationSchema() {
      return postApplicationSchema;
    }
  }
}
