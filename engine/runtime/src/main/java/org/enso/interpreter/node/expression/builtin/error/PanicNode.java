package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin panic function. */
@NodeInfo(shortName = "Panic.throw", description = "Root node for the builtin panic function.")
public class PanicNode extends BuiltinRootNode {
  private PanicNode(Language language) {
    super(language);
  }

  /**
   * Executes this node.
   *
   * <p>Assumes the panic payload is passed as the second argument of the enclosing function and
   * throws it as an exception.
   *
   * @param frame current execution frame
   * @return never returns, always throws an exception
   */
  public Stateful execute(VirtualFrame frame) {
    Object payload = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    throw new PanicException(payload, this);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new PanicNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
