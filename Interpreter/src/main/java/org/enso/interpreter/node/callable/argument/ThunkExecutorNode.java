package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.callable.argument.Thunk;

/** Node responsible for executing (forcing) thunks passed to it as runtime values. */
@NodeField(name = "isTail", type = Boolean.class)
public abstract class ThunkExecutorNode extends Node {

  /**
   * Forces the thunk to its resulting value.
   *
   * @param thunk the thunk to force
   * @return the return value of this thunk
   */
  public abstract Object executeThunk(Thunk thunk);

  abstract boolean getIsTail();

  @Specialization(
      guards = "callNode.getCallTarget() == thunk.getCallTarget()",
      limit = Constants.CacheSizes.THUNK_EXECUTOR_NODE)
  Object doCached(
      Thunk thunk,
      @Cached("create(thunk.getCallTarget())") DirectCallNode callNode,
      @Cached("createLoopingOptimizerIfNeeded()")
          LoopingCallOptimiserNode loopingCallOptimiserNode) {
    if (getIsTail()) {
      return callNode.call(thunk.getScope());
    } else {
      try {
        return callNode.call(thunk.getScope());
      } catch (TailCallException e) {
        return loopingCallOptimiserNode.executeDispatch(e.getFunction(), e.getArguments());
      }
    }
  }

  @Specialization(replaces = "doCached")
  Object doUncached(
      Thunk thunk,
      @Cached IndirectCallNode callNode,
      @Cached("createLoopingOptimizerIfNeeded()")
          LoopingCallOptimiserNode loopingCallOptimiserNode) {
    try {
      return callNode.call(thunk.getCallTarget(), thunk.getScope());
    } catch (TailCallException e) {
      return loopingCallOptimiserNode.executeDispatch(e.getFunction(), e.getArguments());
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
