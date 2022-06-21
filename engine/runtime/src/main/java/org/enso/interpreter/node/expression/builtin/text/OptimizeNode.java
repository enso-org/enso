package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Prim_Text_Helper",
    name = "optimize",
    description = "Forces flattening of a text value, for testing purposes.")
public abstract class OptimizeNode extends Node {
  private @Child ToJavaStringNode toJavaStringNode = ToJavaStringNode.build();

  static OptimizeNode build() {
    return OptimizeNodeGen.create();
  }

  abstract Object execute(Object _this, Object text);

  @Specialization
  Text doText(Object _this, Text text) {
    toJavaStringNode.execute(text);
    return text;
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    return that;
  }
}
