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
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "IO",
    name = "println",
    description = "Prints its argument to standard out.",
    autoRegister = false)
public abstract class PrintlnNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  abstract Object execute(VirtualFrame frame, State state, @AcceptsError Object message);

  @Specialization(guards = "strings.isString(message)")
  Object doPrintText(
      VirtualFrame frame,
      State state,
      Object message,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary strings) {
    EnsoContext ctx = EnsoContext.get(this);
    try {
      print(ctx.getOut(), strings.asString(message));
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
    return ctx.getNothing();
  }

  @Specialization(guards = "!strings.isString(message)")
  Object doPrint(
      VirtualFrame frame,
      State state,
      Object message,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary strings,
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached("buildSymbol()") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode) {
    Object probablyStr = invokeCallableNode.execute(symbol, frame, state, new Object[] {message});
    if (warnings.hasWarnings(probablyStr)) {
      try {
        probablyStr = warnings.removeWarnings(probablyStr);
      } catch (UnsupportedMessageException e) {
        throw CompilerDirectives.shouldNotReachHere(e);
      }
    }

    String str;
    if (strings.isString(probablyStr)) {
      try {
        str = strings.asString(probablyStr);
      } catch (UnsupportedMessageException e) {
        throw CompilerDirectives.shouldNotReachHere(e);
      }
    } else {
      str = fallbackToString(probablyStr);
    }

    EnsoContext ctx = EnsoContext.get(this);
    print(ctx.getOut(), str);
    return ctx.getNothing();
  }

  @CompilerDirectives.TruffleBoundary
  private String fallbackToString(Object obj) {
    return obj.toString();
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream out, Object str) {
    out.println(str);
  }

  @NeverDefault
  UnresolvedSymbol buildSymbol() {
    return UnresolvedSymbol.build("to_text", EnsoContext.get(this).getBuiltins().getScope());
  }

  @NeverDefault
  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  @NeverDefault
  static PrintlnNode build() {
    return PrintlnNodeGen.create();
  }
}
