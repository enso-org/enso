package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNodeGen;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/** A runtime representation of a function object in Enso. */
@ExportLibrary(InteropLibrary.class)
public final class Function implements TruffleObject {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;
  private final ArgumentSchema schema;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] preAppliedArguments;
  private final @CompilationFinal(dimensions = 1) Object[] oversaturatedArguments;

  /**
   * Creates a new function.
   *
   * @param callTarget the target containing the function's code
   * @param scope a frame representing the function's scope
   * @param schema the {@link ArgumentSchema} with which the function was defined
   * @param preappliedArguments the preapplied arguments for this function. The layout of this array
   *     must be conforming to the {@code schema}. {@code null} is allowed if the function does not
   *     have any partially applied arguments.
   */
  public Function(
      RootCallTarget callTarget,
      MaterializedFrame scope,
      ArgumentSchema schema,
      Object[] preappliedArguments,
      Object[] oversaturatedArguments) {
    this.callTarget = callTarget;
    this.scope = scope;
    this.schema = schema;
    this.preAppliedArguments = preappliedArguments;
    this.oversaturatedArguments = oversaturatedArguments;
  }

  /**
   * Creates a new function without any partially applied arguments.
   *
   * @param callTarget the target containing the function's code
   * @param scope a frame representing the function's scope
   * @param schema the {@link ArgumentSchema} with which the function was defined
   */
  public Function(RootCallTarget callTarget, MaterializedFrame scope, ArgumentSchema schema) {
    this(callTarget, scope, schema, null, null);
  }

  /**
   * Gets the target containing the function's code.
   *
   * @return the target containing the function's code
   */
  public RootCallTarget getCallTarget() {
    return callTarget;
  }

  /**
   * Gets the function's scope.
   *
   * @return the function's scope
   */
  public MaterializedFrame getScope() {
    return scope;
  }

  /**
   * Gets the function's argument schema.
   *
   * @return the function's argument schema
   */
  public ArgumentSchema getSchema() {
    return schema;
  }

  /**
   * Obtains the oversaturated arguments associated with this function.
   *
   * @return an array of this function's oversaturated arguments
   */
  public Object[] getOversaturatedArguments() {
    return oversaturatedArguments != null ? oversaturatedArguments : new Object[0];
  }

  /**
   * Gets a copy of the partially applied arguments for this function, safe to be mutated by
   * clients.
   *
   * @return a copy of the partially applied arguments for this function
   */
  public Object[] clonePreAppliedArguments() {
    return preAppliedArguments.clone();
  }

  /**
   * Checks if this runtime object is executable.
   *
   * @return {@code true}
   */
  @ExportMessage
  public boolean isExecutable() {
    return true;
  }

  /**
   * A class representing the executable behaviour of the function.
   *
   * <p>This class gets exposed via the Truffle interop library to allow Enso functions to be called
   * from other guest languages running on GraalVM.
   */
  @ExportMessage
  public abstract static class Execute {

    /**
     * Builds an argument sorter node prepared to apply a predefined number of arguments.
     *
     * @param length the number of arguments to build the sorter node for.
     * @return an argument sorter node ready to apply {@code length} arguments.
     */
    @ExplodeLoop
    protected static ArgumentSorterNode buildSorter(int length) {
      CallArgumentInfo[] args = new CallArgumentInfo[length];
      for (int i = 0; i < length; i++) {
        args[i] = new CallArgumentInfo(null, false, true);
      }
      return ArgumentSorterNodeGen.create(args, false, true);
    }

    /**
     * Calls a function extensively caching relevant metadata. This is the fast path operation.
     *
     * @param function the function to execute
     * @param arguments the arguments passed to {@code function} in the expected positional order
     * @param cachedArgsLength the cached arguments count
     * @param sorterNode the cached argument sorter node for the particular arguments array length.
     * @return the result of executing {@code function} on {@code arguments}
     */
    @Specialization(
        guards = "arguments.length == cachedArgsLength",
        limit = Constants.CacheSizes.FUNCTION_INTEROP_LIBRARY)
    protected static Object callCached(
        Function function,
        Object[] arguments,
        @Cached(value = "arguments.length") int cachedArgsLength,
        @Cached(value = "buildSorter(cachedArgsLength)") ArgumentSorterNode sorterNode) {
      return sorterNode.execute(function, arguments);
    }

    /**
     * Calls a function without any caching. This is the slow path variant.
     *
     * @param function the function to execute.
     * @param arguments the arguments to pass to the {@code function}.
     * @return the result of function application.
     */
    @Specialization(replaces = "callCached")
    protected static Object callUncached(Function function, Object[] arguments) {
      return callCached(function, arguments, arguments.length, buildSorter(arguments.length));
    }
  }

  /**
   * Defines a simple schema for accessing arguments from call targets.
   *
   * <p>As Truffle call targets can only take a simple {@code Object[]}, this class provides a way
   * to get the various necessary pieces of information out of that array.
   */
  public static class ArgumentsHelper {

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * <p>The arguments passed to this function must be in positional order. For more information on
     * how to do this, see {@link
     * org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode}.
     *
     * @param function the function to be called
     * @param positionalArguments the arguments to that function, sorted into positional order
     * @return an array containing the necessary information to call an Enso function
     */
    public static Object[] buildArguments(Function function, Object[] positionalArguments) {
      return new Object[] {function.getScope(), positionalArguments};
    }

    /**
     * Gets the positional arguments out of the array.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     Object[])}
     * @return the positional arguments to the function
     */
    public static Object[] getPositionalArguments(Object[] arguments) {
      return (Object[]) arguments[1];
    }

    /**
     * Gets the function's local scope out of the array.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     Object[])}
     * @return the local scope for the associated function
     */
    public static MaterializedFrame getLocalScope(Object[] arguments) {
      return (MaterializedFrame) arguments[0];
    }
  }
}
