package org.enso.interpreter.node.function;

import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RepeatingNode;
import org.enso.interpreter.optimiser.TailCallException;

/**
 * A version of {@link DispatchNode} that is fully prepared to handle tail calls. Tail calls are
 * handled through exceptions – whenever a tail-recursive call would be executed, an exception
 * containing the next unevaluated call and arguments is thrown instead (see: {@link
 * TailCallException}).
 *
 * <p>This node executes the function in a loop, following all the continuations, until obtaining
 * the actual return value.
 */
public class LoopingDispatchNode extends DispatchNode {

  private final FrameDescriptor loopFrameDescriptor = new FrameDescriptor();
  @Child private LoopNode loopNode;

  public LoopingDispatchNode() {
    loopNode = Truffle.getRuntime().createLoopNode(new RepeatedCallNode(loopFrameDescriptor));
  }

  @Override
  public Object executeDispatch(Object receiver, Object[] arguments) {
    VirtualFrame frame = Truffle.getRuntime().createVirtualFrame(null, loopFrameDescriptor);
    ((RepeatedCallNode) loopNode.getRepeatingNode()).setNextCall(frame, receiver, arguments);
    loopNode.executeLoop(frame);
    return ((RepeatedCallNode) loopNode.getRepeatingNode()).getResult(frame);
  }

  public static final class RepeatedCallNode extends Node implements RepeatingNode {
    private final FrameSlot resultSlot;
    private final FrameSlot functionSlot;
    private final FrameSlot argsSlot;
    @Child private CallNode dispatchNode;

    public RepeatedCallNode(FrameDescriptor descriptor) {
      functionSlot = descriptor.findOrAddFrameSlot("<TCO Function>", FrameSlotKind.Object);
      resultSlot = descriptor.findOrAddFrameSlot("<TCO Result>", FrameSlotKind.Object);
      argsSlot = descriptor.findOrAddFrameSlot("<TCO Arguments>", FrameSlotKind.Object);
      dispatchNode = CallNodeGen.create();
    }

    public void setNextCall(VirtualFrame frame, Object function, Object[] arguments) {
      frame.setObject(functionSlot, function);
      frame.setObject(argsSlot, arguments);
    }

    public Object getResult(VirtualFrame frame) {
      return FrameUtil.getObjectSafe(frame, resultSlot);
    }

    public Object getNextFunction(VirtualFrame frame) {
      Object result = FrameUtil.getObjectSafe(frame, functionSlot);
      frame.setObject(functionSlot, null);
      return result;
    }

    public Object[] getNextArgs(VirtualFrame frame) {
      Object[] result = (Object[]) FrameUtil.getObjectSafe(frame, argsSlot);
      frame.setObject(argsSlot, null);
      return result;
    }

    @Override
    public boolean executeRepeating(VirtualFrame frame) {
      try {
        Object function = getNextFunction(frame);
        Object[] arguments = getNextArgs(frame);
        frame.setObject(resultSlot, dispatchNode.executeCall(function, arguments));
        return false;
      } catch (TailCallException e) {
        setNextCall(frame, e.getFunction(), e.getArguments());
        return true;
      }
    }
  }
}
