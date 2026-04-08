package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectTextNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Text", name = "+", description = "Text concatenation.")
public abstract class ConcatNode extends Node {
  abstract Text execute(Object left, Object right);

  static ConcatNode build() {
    return ConcatNodeGen.create();
  }

  @Specialization
  Text doExecute(
      Object left,
      Object right,
      @Cached ExpectTextNode leftCast,
      @Cached ExpectTextNode rightCast) {
    Text l = leftCast.execute(left);
    Text r = rightCast.execute(right);
    return Text.create(l, r);
  }
}
