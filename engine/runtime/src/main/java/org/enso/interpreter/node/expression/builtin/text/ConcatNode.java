package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Text", name = "+", description = "Text concatenation.")
public class ConcatNode extends Node {
  Text execute(Text _this, Text that) {
    return Text.create(_this, that);
  }
}
