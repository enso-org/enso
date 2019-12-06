package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin State.put function. */
@NodeInfo(shortName = "State.put", description = "Root node for the builtin State.put function")
public class PutStateNode extends BuiltinRootNode {
  private PutStateNode(Language language) {
    super(language);
  }

  /**
   * Executes by taking the desired state value from the second function argument and setting it as
   * the new state.
   *
   * @param frame current execution frame
   * @return The new state value and a modification setting the state to the same value
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object newState = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    return new Stateful(newState, newState);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new PutStateNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "newState", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "State.put";
  }
}
