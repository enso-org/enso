package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.RuntimeError;

/** Root node for the builtin throw error function. */
@NodeInfo(
    shortName = "Error.throw",
    description = "Root node for the builtin throw error function.")
public class ThrowErrorNode extends RootNode {

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
  public Object execute(VirtualFrame frame) {
    Object errorPayload = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    return new RuntimeError(errorPayload);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromRootNode(
        new ThrowErrorNode(language),
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "payload", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
