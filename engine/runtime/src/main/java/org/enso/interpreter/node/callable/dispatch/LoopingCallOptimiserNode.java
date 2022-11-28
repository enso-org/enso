package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RepeatingNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.state.State;

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
@GenerateUncached
public abstract class LoopingCallOptimiserNode extends CallOptimiserNode {
  LoopingCallOptimiserNode() {}

  /**
   * Creates a new instance of this node.
   *
   * @return a new instance of this node.
   */
  public static LoopingCallOptimiserNode build() {
    return LoopingCallOptimiserNodeGen.create();
  }

  /**
   * Calls the provided {@code function} using the provided {@code arguments}.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to {@code function}
   * @param loopNode a cached instance of the loop node used by this node
   * @return the result of executing {@code function} using {@code arguments}
   */
  @Specialization
  public Object dispatch(
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      @Cached(value = "createLoopNode()") LoopNode loopNode) {
    RepeatedCallNode repeatedCallNode = (RepeatedCallNode) loopNode.getRepeatingNode();
    VirtualFrame frame = repeatedCallNode.createFrame();
    repeatedCallNode.setNextCall(frame, function, callerInfo, arguments);
    repeatedCallNode.setState(frame, state);
    loopNode.execute(frame);

    return repeatedCallNode.getResult(frame);
  }

  @Specialization(replaces = "dispatch")
  @CompilerDirectives.TruffleBoundary
  public Object uncachedDispatch(
      Function function,
      CallerInfo callerInfo,
      State state,
      Object[] arguments,
      @Cached ExecuteCallNode executeCallNode) {
    while (true) {
      try {
        return executeCallNode.executeCall(function, callerInfo, state, arguments);
      } catch (TailCallException e) {
        function = e.getFunction();
        callerInfo = e.getCallerInfo();
        arguments = e.getArguments();
      }
    }
  }

  /**
   * Creates a loop node.
   *
   * @return a loop node
   */
  static LoopNode createLoopNode() {
    return Truffle.getRuntime().createLoopNode(new RepeatedCallNode());
  }

  /**
   * This node handles the actually looping computation computed in a tail-recursive function. It
   * will execute the computation repeatedly and is intended to be used in the context of a Truffle
   * {@link RepeatingNode}.
   */
  public static final class RepeatedCallNode extends Node implements RepeatingNode {
    private final int resultSlotIdx;
    private final int functionSlotIdx;
    private final int argsSlotIdx;
    private final int stateSlotIdx;
    private final int callerInfoSlotIdx;
    private final FrameDescriptor descriptor;
    @Child private ExecuteCallNode dispatchNode;

    /** Creates a new node used for repeating a call. */
    public RepeatedCallNode() {
      var descrBuilder = FrameDescriptor.newBuilder();
      functionSlotIdx = descrBuilder.addSlot(FrameSlotKind.Object, "<TCO Function>", null);
      resultSlotIdx = descrBuilder.addSlot(FrameSlotKind.Object, "<TCO Result>", null);
      argsSlotIdx = descrBuilder.addSlot(FrameSlotKind.Object, "<TCO Arguments>", null);
      stateSlotIdx = descrBuilder.addSlot(FrameSlotKind.Object, "<TCO State>", null);
      callerInfoSlotIdx = descrBuilder.addSlot(FrameSlotKind.Object, "<TCO Caller Info>", null);
      descriptor = descrBuilder.build();
      dispatchNode = ExecuteCallNodeGen.create();
    }

    private VirtualFrame createFrame() {
      return Truffle.getRuntime().createVirtualFrame(null, descriptor);
    }

    /**
     * Sets the call target for the next call made using {@code frame}.
     *
     * @param frame the stack frame for execution
     * @param function the function to execute in {@code frame}
     * @param callerInfo the caller info to pass to the function
     * @param arguments the arguments to execute {@code function} with
     */
    private void setNextCall(
        VirtualFrame frame, Function function, CallerInfo callerInfo, Object[] arguments) {
      frame.setObject(functionSlotIdx, function);
      frame.setObject(callerInfoSlotIdx, callerInfo);
      frame.setObject(argsSlotIdx, arguments);
    }

    private void setState(VirtualFrame frame, State state) {
      frame.setObject(stateSlotIdx, state);
    }

    /**
     * Obtains the result of looping execution.
     *
     * @param frame the stack frame for execution
     * @return the result of execution in {@code frame}
     */
    public Object getResult(VirtualFrame frame) {
      return frame.getObject(resultSlotIdx);
    }

    private CallerInfo getCallerInfo(VirtualFrame frame) {
      CallerInfo result = (CallerInfo) frame.getObject(callerInfoSlotIdx);
      frame.setObject(callerInfoSlotIdx, null);
      return result;
    }

    /**
     * Generates the next call to be made during looping execution.
     *
     * @param frame the stack frame for execution
     * @return the function to be executed next in the loop
     */
    public Function getNextFunction(VirtualFrame frame) {
      Object result = frame.getObject(functionSlotIdx);
      frame.setObject(functionSlotIdx, null);
      return (Function) result;
    }

    /**
     * Generates the next state value for the looping function
     *
     * @param frame the stack frame for execution
     * @return the state to pass to the next function
     */
    public Object getNextState(VirtualFrame frame) {
      return frame.getObject(stateSlotIdx);
    }

    /**
     * Generates the next set of arguments to the looping function.
     *
     * @param frame the stack frame for execution
     * @return the arguments to be applied to the next function
     */
    public Object[] getNextArgs(VirtualFrame frame) {
      Object[] result = (Object[]) frame.getObject(argsSlotIdx);
      frame.setObject(argsSlotIdx, null);
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
      com.oracle.truffle.api.TruffleSafepoint.poll(this);
      try {
        Function function = getNextFunction(frame);
        Object state = getNextState(frame);
        Object[] arguments = getNextArgs(frame);
        CallerInfo callerInfo = getCallerInfo(frame);
        frame.setObject(
            resultSlotIdx, dispatchNode.executeCall(function, callerInfo, state, arguments));
        return false;
      } catch (TailCallException e) {
        setNextCall(frame, e.getFunction(), e.getCallerInfo(), e.getArguments());
        return true;
      }
    }
  }
}
