package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin State.get function. */
@NodeInfo(shortName = "State.get", description = "Root node for the builtin State.get function")
public class GetStateNode extends BuiltinRootNode {
  private GetStateNode(Language language) {
    super(language);
  }

  /**
   * Executes the function by accessing the state parameter and returning it.
   *
   * @param frame current execution frame
   * @return the current state value without modifying the state
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, state);
  }

  /**
   * Creates a single-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new GetStateNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "State.get";
  }
}
