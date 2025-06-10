package org.enso.interpreter.node.expression.builtin.thread;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;

@BuiltinMethod(
    type = "Thread",
    name = "with_interrupt_handler",
    description = "Runs a computation with a handler for thread interrupts.",
    autoRegister = false,
    inlineable = true)
public class WithInterruptHandlerNode extends Node {
  private @Child ThunkExecutorNode actExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode handlerExecutorNode = ThunkExecutorNode.build();
  private final BranchProfile interruptBranch = BranchProfile.create();

  Object execute(VirtualFrame frame, @Suspend Object action, @Suspend Object interrupt_handler) {
    var ctx = EnsoContext.get(this);
    var state = ctx.currentState();
    try {
      return actExecutorNode.executeThunk(frame, action, state, BaseNode.TailStatus.NOT_TAIL);
    } catch (ThreadInterruptedException e) {
      interruptBranch.enter();
      handlerExecutorNode.executeThunk(
          frame, interrupt_handler, state, BaseNode.TailStatus.NOT_TAIL);
      throw e;
    }
  }
}
