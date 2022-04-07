package org.enso.interpreter.node.callable.thunk;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

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
  public abstract Stateful executeThunk(Object thunk, Object state, BaseNode.TailStatus isTail);

  boolean sameCallTarget(DirectCallNode callNode, Function function) {
    return function.getCallTarget() == callNode.getCallTarget();
  }

  @Specialization(
      guards = {"thunk.isThunk()", "sameCallTarget(callNode, thunk)"},
      limit = Constants.CacheSizes.THUNK_EXECUTOR_NODE)
  Stateful doCached(
      Function thunk,
      Object state,
      BaseNode.TailStatus isTail,
      @Cached("create(thunk.getCallTarget())") DirectCallNode callNode,
      @Cached LoopingCallOptimiserNode loopingCallOptimiserNode) {
    CompilerAsserts.partialEvaluationConstant(isTail);
    if (isTail != BaseNode.TailStatus.NOT_TAIL) {
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

  @Specialization(replaces = "doCached", guards = "thunk.isThunk()")
  Stateful doUncached(
      Function thunk,
      Object state,
      BaseNode.TailStatus isTail,
      @Cached IndirectCallNode callNode,
      @Cached LoopingCallOptimiserNode loopingCallOptimiserNode) {
    if (isTail != BaseNode.TailStatus.NOT_TAIL) {
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

  @Fallback
  Stateful doOther(Object thunk, Object state, BaseNode.TailStatus isTail) {
    return new Stateful(state, thunk);
  }
}
