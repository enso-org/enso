package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Any", name = "to_text", description = "Generic text conversion.")
public class AnyToTextNode extends Node {
  String execute(Object self) {
    return toText(self);
  }

  @CompilerDirectives.TruffleBoundary
  private String toText(Object txt) {
    return txt.toString();
  }
}
