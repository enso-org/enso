package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.runtime.callable.argument.sentinel.UnappliedArgumentSentinel;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.error.ArgumentMappingException;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * Tracks simple information about call-site arguments, used to make processing of caller argument
 * lists much mpre simple.
 */
public class CallArgumentInfo {
  private final String name;
  private final boolean isNamed;
  private final boolean isPositional;
  private final boolean isIgnored;

  /**
   * Creates the information from a {@link CallArgument}.
   *
   * @param callArgNode the structure to take information from
   */
  public CallArgumentInfo(CallArgument callArgNode) {
    this(
        callArgNode.getName(),
        callArgNode.isNamed(),
        callArgNode.isPositional(),
        callArgNode.isIgnored());
  }

  /**
   * Creates the information explicitly.
   *
   * @param name the name of the argument, if present
   * @param isNamed whether or not the argument is passed by name
   * @param isPositional whether or not the argument is passed by position
   * @param isIgnored whether or not the argument is an ignore
   */
  public CallArgumentInfo(String name, boolean isNamed, boolean isPositional, boolean isIgnored) {
    this.name = name;
    this.isNamed = isNamed;
    this.isPositional = isPositional;
    this.isIgnored = isIgnored;
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
   * Checks whether the argument is ignoring a default value.
   *
   * @return {@code true} if the argument ignores a default, otherwise {@code false}
   */
  public boolean isIgnored() {
    return isIgnored;
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
   * <p>If an argument is explicitly left unapplied (for the purposes of currying), the result array
   * will contain a sentinel value of type {@link
   * UnappliedArgumentSentinel} in that position.
   * This ensures that execution knows not to use a possible default value for this position.
   *
   * @param order a mapping where position {@code i} in the array contains the destination position
   *     in the target array for the calling argument in position {@code i}
   * @param args the function arguments to reorder, ordered as at the call site
   * @param numDefinedArgs the number of arguments the function was defined for
   * @return {@code args} sorted according to the provided {@code order} in an array with slots for
   *     the number of arguments the function was defined for
   */
  @ExplodeLoop
  public static Object[] reorderArguments(int[] order, Object[] args, int numDefinedArgs) {
    Object[] result = new Object[numDefinedArgs]; // Note [Defined Arguments]

    for (int i = 0; i < args.length; i++) {
      result[order[i]] = args[i];
    }
    return result;
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
   *
   * However, this doesn't handle the case where arguments are intentionally ignored, so we use a
   * special sentinel value to handle this.
   */

  /**
   * Generates a mapping between the call-site argument positions and the definition-site argument
   * positions.
   *
   * @param callable the construct being called
   * @param callArgs information on the arguments at the call site, ordered as at the call site
   * @return a mapping where position {@code i} in the array contains the destination position in
   *     the target array for the calling argument in position {@code i}
   */
  public static int[] generateArgMapping(Object callable, CallArgumentInfo[] callArgs) {
    if (TypesGen.isCallable(callable)) {
      Callable realCallable = (Callable) callable;

      ArgumentDefinition[] definedArgs = realCallable.getArgs();
      int numberOfDefinedArgs = definedArgs.length;

      boolean[] definedArgumentIsUsed = new boolean[numberOfDefinedArgs];
      int[] argumentSortOrder = new int[callArgs.length];

      for (int i = 0; i < callArgs.length; ++i) {
        boolean argumentProcessed = false;
        CallArgumentInfo currentArgument = callArgs[i];

        boolean argumentIsPositional = currentArgument.isPositional();

        if (argumentIsPositional) {
          for (int j = 0; j < numberOfDefinedArgs; j++) {
            boolean argumentIsUnused = !definedArgumentIsUsed[j];

            if (argumentIsUnused) {
              argumentSortOrder[i] = j;
              definedArgumentIsUsed[j] = true;
              argumentProcessed = true;
              break;
            }
          }

          if (!argumentProcessed) {
            throw new ArgumentMappingException(realCallable, currentArgument, i);
          }

        } else {
          for (int j = 0; j < numberOfDefinedArgs; j++) {
            boolean argumentIsValidAndNamed =
                currentArgument.getName().equals(definedArgs[j].getName())
                    && !definedArgumentIsUsed[j];

            if (argumentIsValidAndNamed) {
              argumentSortOrder[i] = j;
              definedArgumentIsUsed[j] = true;
              argumentProcessed = true;
              break;
            }
          }

          if (!argumentProcessed) {
            throw new ArgumentMappingException(realCallable, currentArgument, i);
          }
        }
      }

      return argumentSortOrder;
    } else {
      throw new NotInvokableException(callable, null);
    }
  }
}
