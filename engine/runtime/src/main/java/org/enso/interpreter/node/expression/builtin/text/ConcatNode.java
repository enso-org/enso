package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectTextNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Text", name = "+", description = "Text concatenation.")
public abstract class ConcatNode extends Node {
  abstract Text execute(Object _this, Object that);

  static ConcatNode build() {
    return ConcatNodeGen.create();
  }

  @Specialization
  Text doExecute(
      Object _this,
      Object that,
      @Cached ExpectTextNode leftCast,
      @Cached ExpectTextNode rightCast) {
    Text l = leftCast.execute(_this);
    Text r = rightCast.execute(that);
    return Text.create(l, r);
  }
}
