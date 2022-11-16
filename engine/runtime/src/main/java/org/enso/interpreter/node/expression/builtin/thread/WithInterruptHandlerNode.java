package org.enso.interpreter.node.expression.builtin.thread;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Thread",
    name = "with_interrupt_handler",
    description = "Runs a computation with a handler for thread interrupts.",
    autoRegister = false)
public class WithInterruptHandlerNode extends Node {
  private @Child ThunkExecutorNode actExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode handlerExecutorNode = ThunkExecutorNode.build();

  Object execute(State state, @Suspend Object action, @Suspend Object interrupt_handler) {
    try {
      return actExecutorNode.executeThunk(action, state, BaseNode.TailStatus.NOT_TAIL);
    } catch (ThreadInterruptedException e) {
      handlerExecutorNode.executeThunk(interrupt_handler, state, BaseNode.TailStatus.NOT_TAIL);
      throw e;
    }
  }
}
