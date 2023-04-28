package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.node.InlineableNode;

/**
 * This node is responsible for optimising function calls. Where possible, it will handle the call via:
 * <ul>
 *  <li>{@link InlineableNode} to force inlining</li>
 *  <li>{@link DirectCallNode} with potential for inlining</li>
 * </ul>
 */
@NodeInfo(shortName = "ExecCall", description = "Optimises function calls")
@GenerateUncached
public abstract class ExecuteCallNode extends Node {

  ExecuteCallNode() {}

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static ExecuteCallNode build() {
    return ExecuteCallNodeGen.create();
  }

  /**
   * Inlines the function if its root node implements {@link InlineableNode.Root}.
   *
   * @param frame current frame
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param cachedTarget the cached call target for {@code function}
   * @param callNode the cached call node for {@code cachedTarget}
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(guards = {
    "function.getCallTarget() == cachedTarget",
    "callNode != null"
  })
  protected Object callInlineable(
      VirtualFrame frame,
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached("function.getCallTarget()") RootCallTarget cachedTarget,
      @Cached("createInlineableNode(cachedTarget)") InlineableNode callNode) {
    var args = Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments);
    return callNode.call(frame, args);
  }

  /**
   * Calls the function directly.
   *
   * <p>This specialisation comes into play where the call target for the provided function is
   * already cached. This means that the call can be made quickly.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param cachedTarget the cached call target for {@code function}
   * @param callNode the cached call node for {@code cachedTarget}
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(guards = {
    "function.getCallTarget() == cachedTarget",
  })
  protected Object callDirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached("function.getCallTarget()") RootCallTarget cachedTarget,
      @Cached("createDirectCallNode(cachedTarget)") DirectCallNode callNode) {
    var args = Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments);
    return callNode.call(args);
  }

  static InlineableNode createInlineableNode(RootCallTarget t) {
    if (t.getRootNode() instanceof InlineableNode.Root inlineNodeProvider) {
      return inlineNodeProvider.createInlineableNode();
    }
    return null;
  }

  static DirectCallNode createDirectCallNode(RootCallTarget t) {
    return DirectCallNode.create(t);
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
  @Specialization(replaces = { "callDirect", "callInlineable" })
  protected Object callIndirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached IndirectCallNode callNode) {
    return callNode.call(
        function.getCallTarget(),
        Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments));
  }

  /**
   * Executes the function call.
   *
   * @param frame the caller's frame
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state value to pass to the function
   * @param arguments the arguments to be passed to {@code function}
   * @return the result of executing {@code function} on {@code arguments}
   */
  public abstract Object executeCall(
      VirtualFrame frame, Function function, CallerInfo callerInfo, Object state, Object[] arguments);
}
