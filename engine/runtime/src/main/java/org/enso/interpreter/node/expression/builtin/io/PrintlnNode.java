package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.Stateful;

/** Allows for printing arbitrary values to the standard output. */
@NodeInfo(shortName = "IO.println", description = "Prints its argument to standard out.")
public abstract class PrintlnNode extends BuiltinRootNode {
  private final UnresolvedSymbol toTextSym;
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  PrintlnNode(Language language, ModuleScope scope) {
    super(language);
    toTextSym = UnresolvedSymbol.build("to_text", scope);
  }

  @Specialization
  Stateful doPrint(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    Object in = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    Stateful str = invokeCallableNode.execute(toTextSym, frame, state, new Object[] {in});
    print(ctx.getOut(), str.getValue());

    return new Stateful(str.getState(), ctx.getUnit().newInstance());
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream out, Object str) {
    out.println(str);
  }

  /**
   * Creates a {@link Function} object ignoring its first argument and printing the second to the
   * standard output.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language, ModuleScope scope) {
    return Function.fromBuiltinRootNode(
        PrintlnNodeGen.create(language, scope),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "IO.println";
  }
}
