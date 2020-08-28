package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Text", name = "==", description = "Equality on text.")
public class TextEqualsNode extends Node {
  boolean execute(String _this, String that) {
    return _this.equals(that);
  }
}
