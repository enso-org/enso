package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.io.IOException;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.RuntimeError;

@BuiltinMethod(type = "IO", name = "readln", description = "Reads a line from standard in.")
public abstract class ReadlnNode extends Node {
  static ReadlnNode build() {
    return ReadlnNodeGen.create();
  }

  abstract Object execute(Object _this);

  @Specialization
  @TruffleBoundary
  Object doRead(Object _this, @CachedContext(Language.class) Context ctx) {
    try {
      return ctx.getIn().readLine();
    } catch (IOException e) {
      return new RuntimeError("Empty input stream.");
    }
  }
}
