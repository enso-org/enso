package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Boolean", name = "to_text", description = "Boolean to text conversion.")
public class ToTextNode extends Node {
  Text t = Text.create("True");
  Text f = Text.create("False");

  Text execute(boolean _this) {
    return _this ? t : f;
  }
}
