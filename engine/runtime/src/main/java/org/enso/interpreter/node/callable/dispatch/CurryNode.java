package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNodeGen;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.Stateful;

/** Handles runtime function currying and oversaturated (eta-expanded) calls. */
@NodeInfo(description = "Handles runtime currying and eta-expansion")
public class CurryNode extends BaseNode {
  private final FunctionSchema preApplicationSchema;
  private final FunctionSchema postApplicationSchema;
  private final boolean appliesFully;
  private @Child InvokeCallableNode oversaturatedCallableNode;
  private @Child ExecuteCallNode directCall;
  private @Child CallOptimiserNode loopingCall;

  private CurryNode(
      FunctionSchema originalSchema,
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail) {
    setTail(isTail);
    this.preApplicationSchema = originalSchema;
    this.postApplicationSchema = postApplicationSchema;
    appliesFully = isFunctionFullyApplied(defaultsExecutionMode);
    initializeCallNodes();
    initializeOversaturatedCallNode(defaultsExecutionMode, argumentsExecutionMode);
  }

  /**
   * Creates a new instance of this node.
   *
   * @param preApplicationSchema the schema of all functions being used in the {@link
   *     #execute(VirtualFrame, Function, CallerInfo, Object, Object[], Object[])} method.
   * @param argumentMapping the argument mapping for moving from the original schema to the argument
   *     schema expected by the function.
   * @param defaultsExecutionMode the mode of handling defaulted arguments for this call.
   * @param argumentsExecutionMode the mode of executing lazy arguments for this call.
   * @param isTail is this a tail call position?
   * @return an instance of this node.
   */
  public static CurryNode build(
      FunctionSchema preApplicationSchema,
      CallArgumentInfo.ArgumentMapping argumentMapping,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail) {
    return new CurryNode(
        preApplicationSchema,
        argumentMapping.getPostApplicationSchema(),
        defaultsExecutionMode,
        argumentsExecutionMode,
        isTail);
  }

  private void initializeCallNodes() {
    if (postApplicationSchema.hasOversaturatedArgs()
        || !preApplicationSchema.getCallStrategy().shouldCallDirect(isTail())) {
      this.loopingCall = CallOptimiserNode.build();
    } else {
      this.directCall = ExecuteCallNode.build();
    }
  }

  private void initializeOversaturatedCallNode(
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    if (postApplicationSchema.hasOversaturatedArgs()) {
      oversaturatedCallableNode =
          InvokeCallableNodeGen.create(
              postApplicationSchema.getOversaturatedArguments(),
              defaultsExecutionMode,
              argumentsExecutionMode);
      oversaturatedCallableNode.setTail(isTail());
    }
  }

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
   * @return the result of executing the {@code function}.
   */
  public Stateful execute(
      VirtualFrame frame,
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      Object[] oversaturatedArguments) {
    if (appliesFully) {
      if (!postApplicationSchema.hasOversaturatedArgs()) {
        return doCall(function, callerInfo, state, arguments);
      } else {
        Stateful evaluatedVal = loopingCall.executeDispatch(function, callerInfo, state, arguments);

        return this.oversaturatedCallableNode.execute(
            evaluatedVal.getValue(), frame, evaluatedVal.getState(), oversaturatedArguments);
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
      Function function, CallerInfo callerInfo, Object state, Object[] arguments) {
    if (preApplicationSchema.getCallStrategy().shouldCallDirect(isTail())) {
      return directCall.executeCall(function, callerInfo, state, arguments);
    } else if (isTail()) {
      throw new TailCallException(function, callerInfo, state, arguments);
    } else {
      return loopingCall.executeDispatch(function, callerInfo, state, arguments);
    }
  }

  private boolean isFunctionFullyApplied(
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode) {
    boolean functionIsFullyApplied = true;
    for (int i = 0; i < postApplicationSchema.getArgumentsCount(); i++) {
      boolean hasValidDefault =
          postApplicationSchema.hasDefaultAt(i) && !defaultsExecutionMode.isIgnore();
      boolean hasPreappliedArg = postApplicationSchema.hasPreAppliedAt(i);

      if (!(hasValidDefault || hasPreappliedArg)) {
        functionIsFullyApplied = false;
        break;
      }
    }
    return functionIsFullyApplied;
  }
}
