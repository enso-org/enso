package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Text", name = "==", description = "Equality on text.")
public class TextEqualsNode extends Node {
  private @Child ToJavaStringNode leftToString = ToJavaStringNode.build();
  private @Child ToJavaStringNode rightToString = ToJavaStringNode.build();

  boolean execute(Text _this, Text that) {
    return leftToString.execute(_this).equals(rightToString.execute(that));
  }
}
