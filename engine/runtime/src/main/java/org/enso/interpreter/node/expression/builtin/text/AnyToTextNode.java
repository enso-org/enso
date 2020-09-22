package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Text;

@BuiltinMethod(type = "Any", name = "to_text", description = "Generic text conversion.")
public class AnyToTextNode extends Node {
  @CompilerDirectives.TruffleBoundary
  Text execute(Object _this) {
    return Text.create(_this.toString());
  }
}
