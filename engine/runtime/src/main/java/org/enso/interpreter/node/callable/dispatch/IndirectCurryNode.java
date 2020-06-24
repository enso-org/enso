package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.IndirectInvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

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
   * @param originalSchema function schema before the call.
   * @param postApplicationSchema function schema after the call.
   * @param defaultsExecutionMode should default arguments be used for this call.
   * @param argumentsExecutionMode are arguments pre-executed or suspended.
   * @param isTail is the call happening in a tail position.
   * @return the result of executing the {@code function}.
   */
  public abstract Stateful execute(
      MaterializedFrame frame,
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      Object[] oversaturatedArguments,
      FunctionSchema originalSchema,
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail);

  @Specialization
  Stateful doCurry(
      MaterializedFrame frame,
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      Object[] oversaturatedArguments,
      FunctionSchema preApplicationSchema,
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail,
      @Cached ExecuteCallNode directCall,
      @Cached LoopingCallOptimiserNode loopingCall,
      @Cached IndirectInvokeCallableNode oversaturatedCallableNode) {
    boolean appliesFully = postApplicationSchema.isFullyApplied(defaultsExecutionMode);
    if (appliesFully) {
      if (!postApplicationSchema.hasOversaturatedArgs()) {
        Stateful result =
            doCall(
                function,
                callerInfo,
                state,
                arguments,
                preApplicationSchema,
                isTail,
                directCall,
                loopingCall);
        if (defaultsExecutionMode.isExecute() && TypesGen.isFunction(result.getValue())) {
          return oversaturatedCallableNode.execute(
              result.getValue(),
              frame,
              result.getState(),
              new Object[0],
              new CallArgumentInfo[0],
              defaultsExecutionMode,
              argumentsExecutionMode,
              isTail);
        } else {
          return result;
        }
      } else {
        Stateful evaluatedVal = loopingCall.executeDispatch(function, callerInfo, state, arguments);

        return oversaturatedCallableNode.execute(
            evaluatedVal.getValue(),
            frame,
            evaluatedVal.getState(),
            oversaturatedArguments,
            postApplicationSchema.getOversaturatedArguments(),
            defaultsExecutionMode,
            argumentsExecutionMode,
            isTail);
      }
    } else {
      return new Stateful(
          state,
          new Function(
              function.getCallTarget(),
              function.getScope(),
              postApplicationSchema,
              arguments,
              oversaturatedArguments));
    }
  }

  private Stateful doCall(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      FunctionSchema preApplicationSchema,
      boolean isTail,
      ExecuteCallNode directCall,
      CallOptimiserNode loopingCall) {
    if (preApplicationSchema.getCallStrategy().shouldCallDirect(isTail)) {
      return directCall.executeCall(function, callerInfo, state, arguments);
    } else if (isTail) {
      throw new TailCallException(function, callerInfo, state, arguments);
    } else {
      return loopingCall.executeDispatch(function, callerInfo, state, arguments);
    }
  }
}
