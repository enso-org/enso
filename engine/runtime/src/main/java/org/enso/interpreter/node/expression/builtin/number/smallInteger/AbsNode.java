package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;

@BuiltinMethod(type = "Small_Integer", name = "negate", description = "Negation for numbers.")
public abstract class AbsNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  static AbsNode build() {
    return AbsNodeGen.create();
  }

  abstract Object execute(long _this);

  @Specialization(rewriteOn = ArithmeticException.class)
  long doNormal(long _this) {
    if (_this < 0) {
      return Math.negateExact(_this);
    } else {
      return _this;
    }
  }

  @Specialization
  Object doOverflow(long _this) {
    return toEnsoNumberNode.execute(BigIntegerOps.abs(_this));
  }
}
