package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "IO",
    name = "print_err",
    description = "Prints its argument to standard error.",
    autoRegister = false)
public abstract class PrintErrNode extends Node {
  static PrintErrNode build() {
    return PrintErrNodeGen.create();
  }

  abstract Object execute(VirtualFrame frame, @AcceptsError Object message);

  @Specialization(guards = "strings.isString(message)")
  Object doPrintText(
      VirtualFrame frame,
      Object message,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary strings) {
    EnsoContext ctx = EnsoContext.get(this);
    try {
      print(ctx.getErr(), strings.asString(message));
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
    return ctx.getNothing();
  }

  @Specialization(guards = "!strings.isString(message)")
  Object doPrint(
      VirtualFrame frame,
      Object message,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary strings,
      @Cached("buildToTextSymbol($node)") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode,
      @Cached ExpectStringNode expectStringNode) {
    var ctx = EnsoContext.get(this);
    var state = ctx.currentState();
    var str = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    print(ctx.getErr(), expectStringNode.execute(str));
    return ctx.getNothing();
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream err, Object str) {
    err.println(str);
  }

  boolean isText(Object o) {
    return TypesGen.isText(o);
  }

  @NeverDefault
  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  @NeverDefault
  static UnresolvedSymbol buildToTextSymbol(Node where) {
    var mod = Builtins.get(where).getModule();
    var scope = mod.getScope();
    return UnresolvedSymbol.build(Constants.Names.TO_TEXT, scope);
  }
}
