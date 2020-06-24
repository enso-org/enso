package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(type = "IO", name = "println", description = "Prints its argument to standard out.")
public abstract class PrintlnNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  abstract Stateful execute(
      VirtualFrame frame, @MonadicState Object state, Object _this, Object message);

  @Specialization
  Stateful doPrint(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedContext(Language.class) Context ctx,
      @Cached("buildSymbol(ctx)") UnresolvedSymbol symbol) {
    Stateful str = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    print(ctx.getOut(), str.getValue());
    return new Stateful(str.getState(), ctx.getUnit().newInstance());
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream out, Object str) {
    out.println(str);
  }

  UnresolvedSymbol buildSymbol(Context ctx) {
    return UnresolvedSymbol.build("to_text", ctx.getBuiltins().getScope());
  }

  static PrintlnNode build() {
    return PrintlnNodeGen.create();
  }
}
