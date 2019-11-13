package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin throw error function. */
@NodeInfo(
    shortName = "Error.throw",
    description = "Root node for the builtin throw error function.")
public class ThrowErrorNode extends BuiltinRootNode {

  private ThrowErrorNode(Language language) {
    super(language);
  }

  /**
   * Executes the node.
   *
   * <p>Assumes the error payload is provided as the second argument of the enclosing function and
   * wraps it in a {@link RuntimeError}.
   *
   * @param frame current execution frame
   * @return a runtime error wrapped argument
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object errorPayload = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, new RuntimeError(errorPayload));
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new ThrowErrorNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "payload", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
