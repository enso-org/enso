package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
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
      VirtualFrame frame, @MonadicState Object state, Object self, @AcceptsError Object message);

  @Specialization(guards = "strings.isString(message)")
  Stateful doPrintText(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedLibrary(limit = "10") InteropLibrary strings) {
    Context ctx = Context.get(this);
    try {
      print(ctx.getErr(), strings.asString(message));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible. self is guaranteed to be a string");
    }
    return new Stateful(state, ctx.getNothing().newInstance());
  }

  @Specialization(guards = "!strings.isString(message)")
  Stateful doPrint(
      VirtualFrame frame,
      Object state,
      Object self,
      Object message,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @Cached("buildSymbol()") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode,
      @Cached ExpectStringNode expectStringNode) {
    Stateful str = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    Context ctx = Context.get(this);
    print(ctx.getErr(), expectStringNode.execute(str.getValue()));
    return new Stateful(str.getState(), ctx.getNothing().newInstance());
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

  UnresolvedSymbol buildSymbol() {
    return UnresolvedSymbol.build("to_text", Context.get(this).getBuiltins().getScope());
  }
}
