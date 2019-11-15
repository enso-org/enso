package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.Stateful;

/** Node responsible for executing (forcing) thunks passed to it as runtime values. */
@NodeField(name = "isTail", type = Boolean.class)
public abstract class ThunkExecutorNode extends Node {

  /**
   * Forces the thunk to its resulting value.
   *
   * @param thunk the thunk to force
   * @param state the state to pass to the thunk
   * @return the return value of this thunk
   */
  public abstract Stateful executeThunk(Thunk thunk, Object state);

  abstract boolean getIsTail();

  @Specialization(
      guards = "callNode.getCallTarget() == thunk.getCallTarget()",
      limit = Constants.CacheSizes.THUNK_EXECUTOR_NODE)
  Stateful doCached(
      Thunk thunk,
      Object state,
      @Cached("create(thunk.getCallTarget())") DirectCallNode callNode,
      @Cached("createLoopingOptimizerIfNeeded()")
          LoopingCallOptimiserNode loopingCallOptimiserNode) {
    if (getIsTail()) {
      return (Stateful) callNode.call(Function.ArgumentsHelper.buildArguments(thunk, state));
    } else {
      try {
        return (Stateful) callNode.call(Function.ArgumentsHelper.buildArguments(thunk, state));
      } catch (TailCallException e) {
        return loopingCallOptimiserNode.executeDispatch(
            e.getFunction(), e.getCallerInfo(), e.getState(), e.getArguments());
      }
    }
  }

  @Specialization(replaces = "doCached")
  Stateful doUncached(
      Thunk thunk,
      Object state,
      @Cached IndirectCallNode callNode,
      @Cached("createLoopingOptimizerIfNeeded()")
          LoopingCallOptimiserNode loopingCallOptimiserNode) {
    if (getIsTail()) {
      return (Stateful)
          callNode.call(
              thunk.getCallTarget(), Function.ArgumentsHelper.buildArguments(thunk, state));
    } else {
      try {
        return (Stateful)
            callNode.call(
                thunk.getCallTarget(), Function.ArgumentsHelper.buildArguments(thunk, state));
      } catch (TailCallException e) {
        return loopingCallOptimiserNode.executeDispatch(
            e.getFunction(), e.getCallerInfo(), e.getState(), e.getArguments());
      }
    }
  }

  LoopingCallOptimiserNode createLoopingOptimizerIfNeeded() {
    return getIsTail() ? null : new LoopingCallOptimiserNode();
  }

  /**
   * Creates an instance of this node.
   *
   * @param isTail whether or not the thunk is being executed in a tail call position
   * @return an instance of this node
   */
  public static ThunkExecutorNode build(boolean isTail) {
    return ThunkExecutorNodeGen.create(isTail);
  }
}
