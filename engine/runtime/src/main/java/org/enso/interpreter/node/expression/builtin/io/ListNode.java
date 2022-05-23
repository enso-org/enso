package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.EnsoFile;

import java.io.IOException;

@BuiltinMethod(type = "File", name = "list_immediate", description = "")
public class ListNode extends Node {

  Object execute(EnsoFile _this) {
    try {
      return new Array((Object[]) _this.list());
    } catch (IOException e) {
      Context context = Context.get(this);
      Builtins builtins = context.getBuiltins();
      Object payload = builtins.error().makePolyglotError(e);
      throw new PanicException(payload, this);
    }
  }
}
