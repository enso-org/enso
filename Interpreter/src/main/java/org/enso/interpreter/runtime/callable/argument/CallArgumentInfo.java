package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.runtime.callable.function.ArgumentSchema;
import org.enso.interpreter.runtime.error.ArgumentMappingException;

import java.util.OptionalInt;
import java.util.function.Predicate;

/**
 * Tracks simple information about call-site arguments, used to make processing of caller argument
 * lists much more simple.
 */
public class CallArgumentInfo {
  private final String name;
  private final boolean isNamed;
  private final boolean isPositional;

  /**
   * Creates the information from a {@link CallArgument}.
   *
   * @param callArgNode the structure to take information from
   */
  public CallArgumentInfo(CallArgument callArgNode) {
    this(callArgNode.getName(), callArgNode.isNamed(), callArgNode.isPositional());
  }

  /**
   * Creates the information explicitly.
   *
   * @param name the name of the argument, if present
   * @param isNamed whether or not the argument is passed by name
   * @param isPositional whether or not the argument is passed by position
   */
  public CallArgumentInfo(String name, boolean isNamed, boolean isPositional) {
    this.name = name;
    this.isNamed = isNamed;
    this.isPositional = isPositional;
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
    return isNamed;
  }

  /**
   * Checks whether the argument was applied by position or not.
   *
   * @return {@code true} if the argument was applied positionally, otherwise {@code false}
   */
  public boolean isPositional() {
    return isPositional;
  }

  /**
   * Reorders the arguments from the call-site to match the order at the definition site.
   *
   * <p>If an argument is not applied in {@code args}, the resultant array will contain {@code null}
   * in any places where an argument was not applied. This is then handled later at the point of
   * reading the arguments, where {@link
   * org.enso.interpreter.node.callable.argument.ReadArgumentNode} will use the default value for
   * that argument.
   *
   * @param order a mapping where position {@code i} in the array contains the destination position
   *     in the target array for the calling argument in position {@code i}
   * @param args the function arguments to reorder, ordered as at the call site
   * @param result the array in which the reordered arguments should be stored
   */
  @ExplodeLoop
  public static void reorderArguments(int[] order, Object[] args, Object[] result) {
    for (int i = 0; i < order.length; i++) {
      result[order[i]] = args[i];
    }
  }

  /**
   * Represents a mapping between the defined arguments of a function and the call site arguments.
   */
  public static class ArgumentMapping {
    private int[] appliedMapping;
    private ArgumentDefinition[] definitions;
    private CallArgumentInfo[] callArgs;
    private boolean[] argumentUsed;

    /**
     * Creates an unitialized object of this class. This instance is not safe for external use and
     * thus the constructor is private.
     *
     * @param schema the definition site arguments schema
     * @param callArgs the call site arguments schema
     */
    private ArgumentMapping(ArgumentSchema schema, CallArgumentInfo[] callArgs) {
      appliedMapping = new int[callArgs.length];
      this.callArgs = callArgs;
      definitions = schema.getArgumentInfos();
      argumentUsed = schema.cloneHasPreApplied();
    }

    /**
     * Generates a full mapping between the definition site arguments and call site arguments.
     *
     * @param schema the definition site arguments schema
     * @param callArgs the call site arguments schema
     * @return the generated argument mapping
     */
    public static ArgumentMapping generate(ArgumentSchema schema, CallArgumentInfo[] callArgs) {
      ArgumentMapping mapping = new ArgumentMapping(schema, callArgs);
      mapping.processArguments();
      return mapping;
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
      if (!maybePosition.isPresent()) {
        throw new ArgumentMappingException(definitions, callArg, callArgIndex);
      }
      int position = maybePosition.getAsInt();
      appliedMapping[callArgIndex] = position;
      argumentUsed[position] = true;
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
    public int[] getAppliedMapping() {
      return appliedMapping;
    }

    /**
     * Returns an {@link ArgumentSchema} object resulting from filling in all the call site
     * arguments in the original definition site schema.
     *
     * @return the post-application arguments schema
     */
    public ArgumentSchema getPostApplicationSchema() {
      return new ArgumentSchema(definitions, argumentUsed);
    }
  }

  /* Note [Defined Arguments]
   * ~~~~~~~~~~~~~~~~~~~~~~~~
   * In a language where it is possible to both partially-apply functions and to have default
   * function arguments, it is important that we track information about where arguments have and
   * have not been applied.
   *
   * To do this, we take advantage of the fact that Java will `null` initialise an array. As a
   * result, we know that any nulls in the result array must correspond to arguments that haven't
   * been applied.
   */
}
