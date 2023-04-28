package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.IndirectInvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.State;

/**
 * Handles runtime function currying and oversaturated (eta-expanded) calls.
 *
 * <p>This is a slow path node for the uncached operation.
 */
@NodeInfo(description = "Handles runtime currying and eta-expansion")
@GenerateUncached
public abstract class IndirectCurryNode extends Node {

  /**
   * Execute the function call, taking into account currying and eta-expansion.
   *
   * @param frame current execution frame, used as a caller frame if the function requires it.
   * @param function the function to execute.
   * @param callerInfo the caller info to pass to the function.
   * @param state current monadic state.
   * @param arguments the properly ordered arguments to pass to the function.
   * @param oversaturatedArguments any arguments that should be treated as candidates for an
   *     eta-expanded call.
   * @param postApplicationSchema function schema after the call.
   * @param defaultsExecutionMode should default arguments be used for this call.
   * @param argumentsExecutionMode are arguments pre-executed or suspended.
   * @param isTail is the call happening in a tail position.
   * @return the result of executing the {@code function}.
   */
  public abstract Object execute(
      MaterializedFrame frame,
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      Object[] oversaturatedArguments,
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail);

  @Specialization
  Object doCurry(
      MaterializedFrame frame,
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      Object[] oversaturatedArguments,
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached ExecuteCallNode directCall,
      @Cached LoopingCallOptimiserNode loopingCall,
      @Cached IndirectInvokeCallableNode oversaturatedCallableNode) {
    boolean appliesFully = postApplicationSchema.isFullyApplied(defaultsExecutionMode);
    if (appliesFully) {
      if (!postApplicationSchema.hasOversaturatedArgs()) {
        var value =
            doCall(frame, function, callerInfo, state, arguments, isTail, directCall, loopingCall);
        if (defaultsExecutionMode.isExecute()
            && (value instanceof Function || (value instanceof AtomConstructor cons
              && cons.getConstructorFunction().getSchema().isFullyApplied()))) {
          return oversaturatedCallableNode.execute(
              value,
              frame,
              state,
              new Object[0],
              new CallArgumentInfo[0],
              defaultsExecutionMode,
              argumentsExecutionMode,
              isTail);
        } else {
          return value;
        }
      } else {
        var evaluatedVal = loopingCall.executeDispatch(frame, function, callerInfo, state, arguments);

        return oversaturatedCallableNode.execute(
            evaluatedVal,
            frame,
            state,
            oversaturatedArguments,
            postApplicationSchema.getOversaturatedArguments(),
            defaultsExecutionMode,
            argumentsExecutionMode,
            isTail);
      }
    } else {
      return new Function(
              function.getCallTarget(),
              function.getScope(),
              postApplicationSchema,
              arguments,
              oversaturatedArguments);
    }
  }

  private Object doCall(
      VirtualFrame frame,
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      BaseNode.TailStatus isTail,
      ExecuteCallNode directCall,
      CallOptimiserNode loopingCall) {
    switch (isTail) {
      case TAIL_DIRECT:
        return directCall.executeCall(frame, function, callerInfo, state, arguments);
      case TAIL_LOOP:
        throw new TailCallException(function, callerInfo, arguments);
      default:
        return loopingCall.executeDispatch(frame, function, callerInfo, state, arguments);
    }
  }
}
