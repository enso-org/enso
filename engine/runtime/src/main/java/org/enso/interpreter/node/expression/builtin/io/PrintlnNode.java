package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "IO", name = "println", description = "Prints its argument to standard out.")
public abstract class PrintlnNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  abstract Stateful execute(
      VirtualFrame frame, @MonadicState Object state, Object _this, @AcceptsError Object message);

  @Specialization(guards = "strings.isString(message)")
  Stateful doPrintText(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedContext(Language.class) Context ctx,
      @CachedLibrary(limit = "10") InteropLibrary strings) {
    try {
      print(ctx.getOut(), strings.asString(message));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible. self is guaranteed to be a string");
    }
    return new Stateful(state, ctx.getUnit().newInstance());
  }

  @Specialization(guards = "!strings.isString(message)")
  Stateful doPrint(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedContext(Language.class) Context ctx,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @Cached("buildSymbol(ctx)") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode,
      @Cached ExpectStringNode expectStringNode) {
    Stateful str = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    print(ctx.getOut(), expectStringNode.execute(str.getValue()));
    return new Stateful(str.getState(), ctx.getUnit().newInstance());
  }

  boolean isText(Object o) {
    return TypesGen.isText(o);
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream out, Object str) {
    out.println(str);
  }

  UnresolvedSymbol buildSymbol(Context ctx) {
    return UnresolvedSymbol.build("to_text", ctx.getBuiltins().getScope());
  }

  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  static PrintlnNode build() {
    return PrintlnNodeGen.create();
  }
}
