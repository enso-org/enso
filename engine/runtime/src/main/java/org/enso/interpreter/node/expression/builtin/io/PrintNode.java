package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

import java.io.PrintStream;

/** Allows for printing arbitrary values to the standard output. */
@NodeInfo(shortName = "IO.println", description = "Root of the IO.println method.")
public abstract class PrintNode extends BuiltinRootNode {
  PrintNode(Language language) {
    super(language);
  }

  @Specialization
  Stateful doPrint(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    doPrint(ctx.getOut(), Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());

    return new Stateful(state, ctx.getUnit().newInstance());
  }

  @CompilerDirectives.TruffleBoundary
  private void doPrint(PrintStream out, Object object) {
    out.println(object);
  }

  /**
   * Creates a {@link Function} object ignoring its first argument and printing the second to the
   * standard output.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        PrintNodeGen.create(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
