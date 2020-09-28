package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Prim_Text_Helpers",
    name = "optimize",
    description = "Text to text conversion, for API purposes.")
public class OptimizeNode extends Node {
  private @Child ToJavaStringNode toJavaStringNode = ToJavaStringNode.build();

  Text execute(Object _this, Text text) {
    toJavaStringNode.execute(text);
    return text;
  }
}
