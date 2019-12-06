package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This node is responsible for optimising function calls.
 *
 * <p>Where possible, it will make the call as a direct call, with potential for inlining.
 */
@NodeInfo(shortName = "ExecCall", description = "Optimises function calls")
public abstract class ExecuteCallNode extends Node {

  /**
   * Calls the function directly.
   *
   * <p>This specialisation comes into play where the call target for the provided function is
   * already cached. THis means that the call can be made quickly.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param cachedTarget the cached call target for {@code function}
   * @param callNode the cached call node for {@code cachedTarget}
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(guards = "function.getCallTarget() == cachedTarget")
  protected Stateful callDirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached("function.getCallTarget()") RootCallTarget cachedTarget,
      @Cached("create(cachedTarget)") DirectCallNode callNode) {
    return (Stateful)
        callNode.call(
            Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments));
  }

  /**
   * Calls the function with a lookup.
   *
   * <p>This specialisation is used in the case where there is no cached call target for the
   * provided function. This is much slower and should, in general, be avoided.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param callNode the cached call node for making indirect calls
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(replaces = "callDirect")
  protected Stateful callIndirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached IndirectCallNode callNode) {
    return (Stateful)
        callNode.call(
            function.getCallTarget(),
            Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments));
  }

  /**
   * Executes the function call.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state value to pass to the function
   * @param arguments the arguments to be passed to {@code function}
   * @return the result of executing {@code function} on {@code arguments}
   */
  public abstract Stateful executeCall(
      Object function, CallerInfo callerInfo, Object state, Object[] arguments);

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static ExecuteCallNode build() {
    return ExecuteCallNodeGen.create();
  }
}
