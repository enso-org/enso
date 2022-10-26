package org.enso.interpreter.node.callable.thunk;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.State;

/** Node responsible for executing (forcing) thunks passed to it as runtime values. */
@GenerateUncached
@ReportPolymorphism
public abstract class ThunkExecutorNode extends Node {

  ThunkExecutorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static ThunkExecutorNode build() {
    return ThunkExecutorNodeGen.create();
  }

  /**
   * Forces the thunk to its resulting value.
   *
   * @param thunk the thunk to force
   * @param state the state to pass to the thunk
   * @param isTail is the execution happening in a tail-call position
   * @return the return value of this thunk
   */
  public abstract Object executeThunk(Object thunk, State state, BaseNode.TailStatus isTail);

  boolean sameCallTarget(DirectCallNode callNode, Function function) {
    return function.getCallTarget() == callNode.getCallTarget();
  }

  @Specialization(
      guards = {"function.isThunk()", "sameCallTarget(callNode, function)"},
      limit = Constants.CacheSizes.THUNK_EXECUTOR_NODE)
  Object doCached(
      Function function,
      State state,
      BaseNode.TailStatus isTail,
      @Cached("create(function.getCallTarget())") DirectCallNode callNode,
      @Cached LoopingCallOptimiserNode loopingCallOptimiserNode) {
    CompilerAsserts.partialEvaluationConstant(isTail);
    if (isTail != BaseNode.TailStatus.NOT_TAIL) {
      return callNode.call(Function.ArgumentsHelper.buildArguments(function, state));
    } else {
      try {
        return callNode.call(Function.ArgumentsHelper.buildArguments(function, state));
      } catch (TailCallException e) {
        return loopingCallOptimiserNode.executeDispatch(
            e.getFunction(), e.getCallerInfo(), state, e.getArguments());
      }
    }
  }

  @Specialization(replaces = "doCached", guards = "function.isThunk()")
  Object doUncached(
      Function function,
      State state,
      BaseNode.TailStatus isTail,
      @Cached IndirectCallNode callNode,
      @Cached LoopingCallOptimiserNode loopingCallOptimiserNode) {
    if (isTail != BaseNode.TailStatus.NOT_TAIL) {
      return callNode.call(
          function.getCallTarget(), Function.ArgumentsHelper.buildArguments(function, state));
    } else {
      try {
        return callNode.call(
            function.getCallTarget(), Function.ArgumentsHelper.buildArguments(function, state));
      } catch (TailCallException e) {
        return loopingCallOptimiserNode.executeDispatch(
            e.getFunction(), e.getCallerInfo(), state, e.getArguments());
      }
    }
  }

  static InvokeFunctionNode buildInvokeFunctionNode(BaseNode.TailStatus tailStatus) {
    var node =
        InvokeFunctionNode.build(
            new CallArgumentInfo[0],
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);
    node.setTailStatus(tailStatus);
    return node;
  }

  static int numberOfTailStatuses() {
    return BaseNode.TailStatus.numberOfValues();
  }

  @Specialization(
      guards = {"!fn.isThunk()", "fn.isFullyApplied()", "isTail == cachedIsTail"},
      limit = "numberOfTailStatuses()")
  Object doCachedFn(
      Function fn,
      State state,
      BaseNode.TailStatus isTail,
      @Cached("isTail") BaseNode.TailStatus cachedIsTail,
      @Cached("buildInvokeFunctionNode(cachedIsTail)") InvokeFunctionNode invokeFunctionNode) {
    return invokeFunctionNode.execute(fn, null, state, new Object[0]);
  }

  @Specialization(
      guards = {"!fn.isThunk()", "fn.isFullyApplied()"},
      replaces = {"doCachedFn"})
  Object doUncachedFn(
      Function fn,
      State state,
      BaseNode.TailStatus isTail,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    return invokeFunctionNode.execute(
        fn,
        null,
        state,
        new Object[0],
        new CallArgumentInfo[0],
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.EXECUTE,
        isTail);
  }

  @Fallback
  Object doOther(Object thunk, State state, BaseNode.TailStatus isTail) {
    return thunk;
  }
}
