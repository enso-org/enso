package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition.ExecutionMode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "IO.print_err", description = "Prints its argument to standard error.")
public abstract class PrintErrNode extends BuiltinRootNode {
  PrintErrNode(Language language) {
    super(language);
  }

  @Specialization
  Stateful doPrint(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    print(ctx.getErr(), Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());

    return new Stateful(state, ctx.getUnit().newInstance());
  }

  @TruffleBoundary
  private void print(PrintStream err, Object object) {
    err.println(object);
  }

  /**
   * Creates a {@link Function} object ignoring its first argument and printing the second to the
   * standard error stream.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        PrintErrNodeGen.create(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ExecutionMode.EXECUTE));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of this node
   */
  @Override
  public String getName() {
    return "IO.print_err";
  }
}
