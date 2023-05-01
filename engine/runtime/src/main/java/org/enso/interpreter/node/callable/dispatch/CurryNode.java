package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNodeGen;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.State;

import java.util.concurrent.locks.Lock;

/** Handles runtime function currying and oversaturated (eta-expanded) calls. */
@NodeInfo(description = "Handles runtime currying and eta-expansion")
public class CurryNode extends BaseNode {
  private final FunctionSchema postApplicationSchema;
  private final boolean appliesFully;
  private @Child InvokeCallableNode oversaturatedCallableNode;
  private @Child ExecuteCallNode directCall;
  private @Child CallOptimiserNode loopingCall;
  private final BranchProfile keepExecutingProfile = BranchProfile.create();
  private final InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode;

  private CurryNode(
      FunctionSchema postApplicationSchema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    setTailStatus(isTail);
    this.defaultsExecutionMode = defaultsExecutionMode;
    this.postApplicationSchema = postApplicationSchema;
    appliesFully = postApplicationSchema.isFullyApplied(defaultsExecutionMode);
    initializeCallNodes();
    initializeOversaturatedCallNode(argumentsExecutionMode);
  }

  /**
   * Creates a new instance of this node.
   *
   * @param argumentMapping the argument mapping for moving from the original schema to the argument
   *     schema expected by the function.
   * @param defaultsExecutionMode the mode of handling defaulted arguments for this call.
   * @param argumentsExecutionMode the mode of executing lazy arguments for this call.
   * @param tailStatus is this a tail call position?
   * @return an instance of this node.
   */
  public static CurryNode build(
      CallArgumentInfo.ArgumentMapping argumentMapping,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus tailStatus) {
    return new CurryNode(
        argumentMapping.getPostApplicationSchema(),
        defaultsExecutionMode,
        argumentsExecutionMode,
        tailStatus);
  }

  private void initializeCallNodes() {
    if (postApplicationSchema.hasOversaturatedArgs() || getTailStatus() == TailStatus.NOT_TAIL) {
      this.loopingCall = CallOptimiserNode.build();
    } else {
      this.directCall = ExecuteCallNode.build();
    }
  }

  private void initializeOversaturatedCallNode(
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    if (postApplicationSchema.hasOversaturatedArgs()) {
      oversaturatedCallableNode =
          InvokeCallableNodeGen.create(
              postApplicationSchema.getOversaturatedArguments(),
              defaultsExecutionMode,
              argumentsExecutionMode);
      oversaturatedCallableNode.setTailStatus(getTailStatus());
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
  public Object execute(
      VirtualFrame frame,
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      Object[] oversaturatedArguments) {
    if (appliesFully) {
      if (!postApplicationSchema.hasOversaturatedArgs()) {
        var value = doCall(frame, function, callerInfo, state, arguments);
        if (defaultsExecutionMode.isExecute()
            && (value instanceof Function || (value instanceof AtomConstructor cons
              && cons.getConstructorFunction().getSchema().isFullyApplied()))) {
          keepExecutingProfile.enter();
          if (oversaturatedCallableNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            Lock lock = getLock();
            lock.lock();
            try {
              if (oversaturatedCallableNode == null) {
                oversaturatedCallableNode =
                    insert(
                        InvokeCallableNode.build(
                            new CallArgumentInfo[0],
                            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
                            InvokeCallableNode.ArgumentsExecutionMode.EXECUTE));
              }
            } finally {
              lock.unlock();
            }
          }

          return oversaturatedCallableNode.execute(value, frame, state, new Object[0]);
        } else {
          return value;
        }
      } else {
        var evaluatedVal = loopingCall.executeDispatch(frame, function, callerInfo, state, arguments);

        return this.oversaturatedCallableNode.execute(
            evaluatedVal, frame, state, oversaturatedArguments);
      }
    } else {
      return
          new Function(
              function.getCallTarget(),
              function.getScope(),
              postApplicationSchema,
              arguments,
              oversaturatedArguments);
    }
  }

  private Object doCall(
      VirtualFrame frame, Function function, CallerInfo callerInfo, State state, Object[] arguments) {
    return switch (getTailStatus()) {
      case TAIL_DIRECT -> directCall.executeCall(frame, function, callerInfo, state, arguments);
      case TAIL_LOOP -> throw new TailCallException(function, callerInfo, arguments);
      default -> loopingCall.executeDispatch(frame, function, callerInfo, state, arguments);
    };
  }
}
