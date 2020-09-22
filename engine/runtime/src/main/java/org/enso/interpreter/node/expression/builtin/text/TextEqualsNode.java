package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Text;

@BuiltinMethod(type = "Text", name = "==", description = "Equality on text.")
public class TextEqualsNode extends Node {
  boolean execute(Text _this, Text that) {
    return _this.equals(that);
  }
}
