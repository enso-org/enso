package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "IO",
    name = "print_err",
    description = "Prints its argument to standard error.")
public abstract class PrintErrNode extends Node {
  static PrintErrNode build() {
    return PrintErrNodeGen.create();
  }

  abstract Stateful execute(
      VirtualFrame frame, @MonadicState Object state, Object _this, @AcceptsError Object message);

  @Specialization
  Stateful doPrintText(
      VirtualFrame frame,
      Object state,
      Object self,
      Text message,
      @CachedContext(Language.class) Context ctx,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    print(ctx.getErr(), toJavaStringNode.execute(message));
    return new Stateful(state, ctx.getUnit().newInstance());
  }

  @Specialization(guards = "!isText(message)")
  Stateful doPrint(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedContext(Language.class) Context ctx,
      @Cached("buildSymbol(ctx)") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    Stateful str = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    print(ctx.getErr(), toJavaStringNode.execute((Text) str.getValue()));
    return new Stateful(str.getState(), ctx.getUnit().newInstance());
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream err, Object str) {
    err.println(str);
  }

  boolean isText(Object o) {
    return TypesGen.isText(o);
  }

  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  UnresolvedSymbol buildSymbol(Context ctx) {
    return UnresolvedSymbol.build("to_text", ctx.getBuiltins().getScope());
  }
}
