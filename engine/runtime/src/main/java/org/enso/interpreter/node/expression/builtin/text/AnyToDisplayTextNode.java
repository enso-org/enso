package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Any", name = "to_display_text")
public abstract class AnyToDisplayTextNode extends Node {
  static AnyToDisplayTextNode build() {
    return AnyToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doShowType(Object self, @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return Text.create(typeToDisplayTextNode.execute(self));
  }
}
