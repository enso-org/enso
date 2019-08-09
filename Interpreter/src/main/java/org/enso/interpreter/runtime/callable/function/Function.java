package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/** A runtime representation of a function object in Enso. */
@ExportLibrary(InteropLibrary.class)
public final class Function extends Callable implements TruffleObject {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;

  /**
   * Creates a new function.
   *
   * @param callTarget the target containing the function's code
   * @param scope a frame representing the function's scope
   * @param arguments the arguments with which the function was defined
   */
  public Function(
      RootCallTarget callTarget, MaterializedFrame scope, ArgumentDefinition[] arguments) {
    super(arguments);
    this.callTarget = callTarget;
    this.scope = scope;
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
     * Calls the function directly.
     *
     * <p>This specialisation comes into play where the call target for the provided function is
     * already cached. THis means that the call can be made quickly.
     *
     * @param function the function to execute
     * @param arguments the arguments passed to {@code function} in the expected positional order
     * @param cachedTarget the cached call target for {@code function}
     * @param callNode the cached call node for {@code cachedTarget}
     * @return the result of executing {@code function} on {@code arguments}
     */
    @Specialization(guards = "function.getCallTarget() == cachedTarget")
    protected static Object callDirect(
        Function function,
        Object[] arguments,
        @Cached("function.getCallTarget()") RootCallTarget cachedTarget,
        @Cached("create(cachedTarget)") DirectCallNode callNode) {
      return callNode.call(function.getScope(), arguments);
    }

    /**
     * Calls the function with a lookup.
     *
     * <p>This specialisation is used in the case where there is no cached call target for the
     * provided function. This is much slower and should, in general, be avoided.
     *
     * @param function the function to execute
     * @param arguments the arguments passed to {@code function} in the expected positional order
     * @param callNode the cached call node for making indirect calls
     * @return the result of executing {@code function} on {@code arguments}
     */
    @Specialization(replaces = "callDirect")
    protected static Object callIndirect(
        Function function, Object[] arguments, @Cached IndirectCallNode callNode) {
      return callNode.call(function.getCallTarget(), function.getScope(), arguments);
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
