package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.state.State;

import java.io.PrintStream;

@BuiltinMethod(
    type = "IO",
    name = "println_builtin",
    description = "Prints its argument to standard out.",
    autoRegister = false)
public abstract class PrintlnNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[]{new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  abstract Object execute(VirtualFrame frame, State state, @AcceptsWarning @AcceptsError Object message, boolean warnings);

  @Specialization(guards = {"strings.isString(message)", "!hasWarningsToPrint(warningsLibrary, warnings, message)"})
  Object doPrintText(
      VirtualFrame frame,
      State state,
      Object message,
      boolean warnings,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @CachedLibrary(limit = "10") WarningsLibrary warningsLibrary) {
    EnsoContext ctx = EnsoContext.get(this);
    try {
      print(ctx.getOut(), strings.asString(message));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible. `message` is guaranteed to be a string");
    }
    return ctx.getNothing();
  }

  @Specialization(guards = {"!strings.isString(message)", "!hasWarningsToPrint(warningsLibrary, warnings, message)"})
  Object doPrint(
      VirtualFrame frame,
      State state,
      Object message,
      boolean warnings,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @CachedLibrary(limit = "10") WarningsLibrary warningsLibrary,
      @Cached("buildSymbol()") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode) {
    Object probablyStr = invokeCallableNode.execute(symbol, frame, state, new Object[]{message});
    Object withoutWarnings = warningsLibrary.removeWarnings(probablyStr);
    String resultAsString = withoutWarnings.toString();
    EnsoContext ctx = EnsoContext.get(this);
    print(ctx.getOut(), resultAsString);
    return ctx.getNothing();
  }

  @Specialization(guards = {"strings.isString(message)", "hasWarningsToPrint(warningsLibrary, warnings, message)"})
  Object doPrintTextWithWarnings(
      VirtualFrame frame,
      State state,
      Object message,
      boolean warnings,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @CachedLibrary(limit = "10") WarningsLibrary warningsLibrary) {
    EnsoContext ctx = EnsoContext.get(this);
    try {
      String str = strings.asString(message) + " (" + warningsLibrary.countWarnings(message) + " attached warnings)";
      print(ctx.getOut(), str);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible. `message` is guaranteed to be a string");
    }
    return ctx.getNothing();
  }

  @Specialization(guards = {"!strings.isString(message)", "hasWarningsToPrint(warningsLibrary, warnings, message)"})
  Object doPrintWithWarnings(
      VirtualFrame frame,
      State state,
      Object message,
      boolean warnings,
      @CachedLibrary(limit = "10") InteropLibrary strings,
      @CachedLibrary(limit = "10") WarningsLibrary warningsLibrary,
      @Cached("buildSymbol()") UnresolvedSymbol symbol,
      @Cached("buildInvokeCallableNode()") InvokeCallableNode invokeCallableNode) {
    Object probablyStr = invokeCallableNode.execute(symbol, frame, state, new Object[]{message});
    Object withoutWarnings = warningsLibrary.removeWarnings(probablyStr);
    String resultAsString = withoutWarnings.toString();
    String str = resultAsString + " (" + warningsLibrary.countWarnings(message) + " " + "attached warnings)";
    EnsoContext ctx = EnsoContext.get(this);
    print(ctx.getOut(), str);
    return ctx.getNothing();
  }

  static boolean hasWarningsToPrint(WarningsLibrary warningsLibrary, boolean shouldPrintWarnings, Object obj) {
    return shouldPrintWarnings && warningsLibrary.hasWarnings(obj);
  }

  @CompilerDirectives.TruffleBoundary
  private void print(PrintStream out, String str) {
    out.println(str);
  }

  UnresolvedSymbol buildSymbol() {
    return UnresolvedSymbol.build("to_text", EnsoContext.get(this).getBuiltins().getScope());
  }

  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[]{new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  static PrintlnNode build() {
    return PrintlnNodeGen.create();
  }
}
