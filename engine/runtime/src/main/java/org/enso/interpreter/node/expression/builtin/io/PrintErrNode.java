package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.io.PrintStream;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(
    type = "IO",
    name = "print_err",
    description = "Prints its argument to standard error.")
public abstract class PrintErrNode extends Node {
  static PrintErrNode build() {
    return PrintErrNodeGen.create();
  }

  abstract Object execute(Object _this, Object message);

  @Specialization
  @TruffleBoundary
  Object doPrint(Object self, Object message, @CachedContext(Language.class) Context ctx) {
    ctx.getErr().println(message);
    return ctx.getUnit().newInstance();
  }
}
