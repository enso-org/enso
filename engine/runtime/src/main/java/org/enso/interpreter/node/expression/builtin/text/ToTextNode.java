package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Text", name = "to_text", description = "Text to text conversion, for API purposes.")
public class ToTextNode extends Node {
  Text execute(Text _this) {
    return _this;
  }
}
