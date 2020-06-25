package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Any", name = "to_text", description = "Generic text conversion.")
public class AnyToTextNode extends Node {
  @CompilerDirectives.TruffleBoundary
  String execute(Object _this) {
    return _this.toString();
  }
}
