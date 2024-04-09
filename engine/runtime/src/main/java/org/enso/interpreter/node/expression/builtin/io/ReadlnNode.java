package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.io.IOException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "IO",
    name = "readln",
    description = "Reads a line from standard in.",
    autoRegister = false)
public abstract class ReadlnNode extends Node {
  static ReadlnNode build() {
    return ReadlnNodeGen.create();
  }

  abstract Object execute();

  @Specialization
  @TruffleBoundary
  Text doRead() {
    try {
      return Text.create(EnsoContext.get(this).getInReader().readLine());
    } catch (IOException e) {
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, null, e);
    }
  }
}
