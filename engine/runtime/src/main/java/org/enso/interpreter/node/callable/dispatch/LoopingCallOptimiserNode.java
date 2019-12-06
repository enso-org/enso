package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RepeatingNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * A version of {@link CallOptimiserNode} that is fully prepared to handle tail calls. Tail calls
 * are handled through exceptions – whenever a tail-recursive call would be executed, an exception
 * containing the next unevaluated call and arguments is thrown instead.
 *
 * <p>This node executes the function in a loop, following all the continuations, until obtaining
 * the actual return value.
 *
 * @see TailCallException
 */
@NodeInfo(shortName = "LoopCall", description = "Handles tail-call elimination")
public class LoopingCallOptimiserNode extends CallOptimiserNode {
  private final FrameDescriptor loopFrameDescriptor = new FrameDescriptor();
  @Child private LoopNode loopNode;

  /** Creates a new node for computing tail-call-optimised functions. */
  public LoopingCallOptimiserNode() {
    loopNode = Truffle.getRuntime().createLoopNode(new RepeatedCallNode(loopFrameDescriptor));
  }

  /**
   * Calls the provided {@code function} using the provided {@code arguments}.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to {@code function}
   * @return the result of executing {@code function} using {@code arguments}
   */
  @Override
  public Stateful executeDispatch(
      Object function, CallerInfo callerInfo, Object state, Object[] arguments) {
    VirtualFrame frame = Truffle.getRuntime().createVirtualFrame(null, loopFrameDescriptor);
    ((RepeatedCallNode) loopNode.getRepeatingNode())
        .setNextCall(frame, function, callerInfo, state, arguments);
    loopNode.execute(frame);

    return ((RepeatedCallNode) loopNode.getRepeatingNode()).getResult(frame);
  }

  /**
   * This node handles the actually looping computation computed in a tail-recursive function. It
   * will execute the computation repeatedly and is intended to be used in the context of a Truffle
   * {@link RepeatingNode}.
   */
  public static final class RepeatedCallNode extends Node implements RepeatingNode {
    private final FrameSlot resultSlot;
    private final FrameSlot functionSlot;
    private final FrameSlot argsSlot;
    private final FrameSlot stateSlot;
    private final FrameSlot callerInfoSlot;
    @Child private ExecuteCallNode dispatchNode;

    /**
     * Creates a new node used for repeating a call.
     *
     * @param descriptor a handle to the slots of the current stack frame
     */
    public RepeatedCallNode(FrameDescriptor descriptor) {
      functionSlot = descriptor.findOrAddFrameSlot("<TCO Function>", FrameSlotKind.Object);
      resultSlot = descriptor.findOrAddFrameSlot("<TCO Result>", FrameSlotKind.Object);
      argsSlot = descriptor.findOrAddFrameSlot("<TCO Arguments>", FrameSlotKind.Object);
      stateSlot = descriptor.findOrAddFrameSlot("<TCO State>", FrameSlotKind.Object);
      callerInfoSlot = descriptor.findOrAddFrameSlot("<TCO Caller Info>", FrameSlotKind.Object);
      dispatchNode = ExecuteCallNodeGen.create();
    }

    /**
     * Sets the call target for the next call made using {@code frame}.
     *
     * @param frame the stack frame for execution
     * @param function the function to execute in {@code frame}
     * @param callerInfo the caller info to pass to the function
     * @param state the state to pass to the function
     * @param arguments the arguments to execute {@code function} with
     */
    public void setNextCall(
        VirtualFrame frame,
        Object function,
        CallerInfo callerInfo,
        Object state,
        Object[] arguments) {
      frame.setObject(functionSlot, function);
      frame.setObject(callerInfoSlot, callerInfo);
      frame.setObject(stateSlot, state);
      frame.setObject(argsSlot, arguments);
    }

    /**
     * Obtains the result of looping execution.
     *
     * @param frame the stack frame for execution
     * @return the result of execution in {@code frame}
     */
    public Stateful getResult(VirtualFrame frame) {
      return (Stateful) FrameUtil.getObjectSafe(frame, resultSlot);
    }

    private CallerInfo getCallerInfo(VirtualFrame frame) {
      CallerInfo result = (CallerInfo) FrameUtil.getObjectSafe(frame, callerInfoSlot);
      frame.setObject(callerInfoSlot, null);
      return result;
    }

    /**
     * Generates the next call to be made during looping execution.
     *
     * @param frame the stack frame for execution
     * @return the function to be executed next in the loop
     */
    public Object getNextFunction(VirtualFrame frame) {
      Object result = FrameUtil.getObjectSafe(frame, functionSlot);
      frame.setObject(functionSlot, null);
      return result;
    }

    /**
     * Generates the next state value for the looping function
     *
     * @param frame the stack frame for execution
     * @return the state to pass to the next function
     */
    public Object getNextState(VirtualFrame frame) {
      Object result = FrameUtil.getObjectSafe(frame, stateSlot);
      frame.setObject(stateSlot, null);
      return result;
    }

    /**
     * Generates the next set of arguments to the looping function.
     *
     * @param frame the stack frame for execution
     * @return the arguments to be applied to the next function
     */
    public Object[] getNextArgs(VirtualFrame frame) {
      Object[] result = (Object[]) FrameUtil.getObjectSafe(frame, argsSlot);
      frame.setObject(argsSlot, null);
      return result;
    }

    /**
     * Executes the node in a repeating fashion until the call is complete.
     *
     * @param frame the stack frame for execution
     * @return {@code true} if execution is continuing, {@code false} otherwise
     */
    @Override
    public boolean executeRepeating(VirtualFrame frame) {
      try {
        Object function = getNextFunction(frame);
        Object state = getNextState(frame);
        Object[] arguments = getNextArgs(frame);
        CallerInfo callerInfo = getCallerInfo(frame);
        frame.setObject(
            resultSlot, dispatchNode.executeCall(function, callerInfo, state, arguments));
        return false;
      } catch (TailCallException e) {
        setNextCall(frame, e.getFunction(), e.getCallerInfo(), e.getState(), e.getArguments());
        return true;
      }
    }
  }
}
