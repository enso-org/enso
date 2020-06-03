package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** Root node for the builtin catch panic function. */
@NodeInfo(
    shortName = "Panic.catch",
    description = "Root node for the builtin catch panic function.")
public class CatchPanicNode extends BuiltinRootNode {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build(false);

  private CatchPanicNode(Language language) {
    super(language);
  }

  /**
   * Executes the node.
   *
   * <p>Assumes the suspended, possibly-panicking computation is passed as the second argument and
   * executes it, catching any {@link PanicException}s.
   *
   * @param frame current execution frame
   * @return the result of the computation if it didn't throw, or a {@link RuntimeError} containing
   *     the thrown panic's payload.
   */
  public Stateful execute(VirtualFrame frame) {
    Object maybeThunk = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    if (TypesGen.isThunk(maybeThunk)) {
      try {
        return thunkExecutorNode.executeThunk(TypesGen.asThunk(maybeThunk), state);
      } catch (PanicException e) {
        return new Stateful(state, new RuntimeError(e.getExceptionObject()));
      }
    } else {
      return new Stateful(state, maybeThunk);
    }
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new CatchPanicNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ArgumentDefinition.ExecutionMode.PASS_THUNK));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "Panic.catch";
  }
}
