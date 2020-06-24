package org.enso.interpreter.node.expression.builtin.thread;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(
    shortName = "Thread.with_interrupt_handler",
    description = "Runs a computation with a handler for thread interrupts.")
public class WithInterruptHandlerNode extends BuiltinRootNode {
  private WithInterruptHandlerNode(Language language) {
    super(language);
  }

  private @Child ThunkExecutorNode actExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode handlerExecutorNode = ThunkExecutorNode.build();

  /**
   * Executes the function.
   *
   * @param frame current execution frame.
   * @return the result of running the function.
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Thunk act = (Thunk) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    Thunk handler =
        (Thunk) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[2];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    try {
      return actExecutorNode.executeThunk(act, state, false);
    } catch (ThreadInterruptedException e) {
      handlerExecutorNode.executeThunk(handler, state, false);
      throw e;
    }
  }

  /**
   * Creates a three-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new WithInterruptHandlerNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "action", ArgumentDefinition.ExecutionMode.PASS_THUNK),
        new ArgumentDefinition(2, "handler", ArgumentDefinition.ExecutionMode.PASS_THUNK));
  }

  @Override
  public String getName() {
    return "Thread.with_interrupt_handler";
  }
}
