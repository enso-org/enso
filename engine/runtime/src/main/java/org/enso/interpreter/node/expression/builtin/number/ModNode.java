package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;

@NodeInfo(shortName = "Number.%", description = "Mod for numbers.")
public class ModNode extends NumberBinaryOpMethod {
  private ModNode(Language language) {
    super(language);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new ModNode(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "that", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Calculates the remainder from dividing the node's operands.
   *
   * @param thisArg the left operand (this)
   * @param thatArg the right operand (that)
   * @return the remainder when dividing {@code thisArg} by {@code thatArg}
   */
  @Override
  protected long op(long thisArg, long thatArg) {
    return thisArg % thatArg;
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Number.%";
  }
}
