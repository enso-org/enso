package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "Number.negate", description = "Negation for numbers.")
public class NegateNode extends BuiltinRootNode {
  private NegateNode(Language language) {
    super(language);
  }

  /**
   * Creates a one-argument function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new NegateNode(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Executes this node.
   *
   * @param frame current execution frame
   * @return the result of negating the function argument
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    long thisArg =
        TypesGen.asLong(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);

    Object state = Function.ArgumentsHelper.getState(frame.getArguments());

    return new Stateful(state, -thisArg);
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Number.negate";
  }
}
