package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;

@BuiltinMethod(type = "Small_Integer", name = "negate", description = "Negation for numbers.")
public abstract class NegateNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  static NegateNode build() {
    return NegateNodeGen.create();
  }

  abstract Object execute(long self);

  @Specialization(rewriteOn = ArithmeticException.class)
  long doNormal(long self) {
    return Math.negateExact(self);
  }

  @Specialization
  Object doOverflow(long self) {
    return toEnsoNumberNode.execute(BigIntegerOps.negate(self));
  }
}
