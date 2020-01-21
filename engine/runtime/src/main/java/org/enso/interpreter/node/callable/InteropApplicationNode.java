package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;

/** A helper node to handle function application for the interop library. */
@GenerateUncached
@NodeInfo(description = "Helper node to handle function application through the interop library.")
public abstract class InteropApplicationNode extends Node {

  /**
   * Calls the function with given state and arguments.
   *
   * @param function the function to call.
   * @param state the current monadic state.
   * @param arguments the arguments for the function.
   * @return the result of calling the function.
   */
  public abstract Object execute(Function function, Object state, Object[] arguments);

  @ExplodeLoop
  InvokeFunctionNode buildSorter(int length) {
    CallArgumentInfo[] args = new CallArgumentInfo[length];
    for (int i = 0; i < length; i++) {
      args[i] = new CallArgumentInfo();
    }
    return InvokeFunctionNode.build(
        args,
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  @Specialization(
      guards = "arguments.length == cachedArgsLength",
      limit = Constants.CacheSizes.FUNCTION_INTEROP_LIBRARY)
  Object callCached(
      Function function,
      Object state,
      Object[] arguments,
      @Cached(value = "arguments.length") int cachedArgsLength,
      @Cached(value = "buildSorter(cachedArgsLength)") InvokeFunctionNode sorterNode) {
    return sorterNode.execute(function, null, state, arguments).getValue();
  }

  @Specialization(replaces = "callCached")
  Object callUncached(Function function, Object state, Object[] arguments) {
    return callCached(function, state, arguments, arguments.length, buildSorter(arguments.length));
  }
}
