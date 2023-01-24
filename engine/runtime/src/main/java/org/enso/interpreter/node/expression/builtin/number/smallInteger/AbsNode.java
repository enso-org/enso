package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;

@BuiltinMethod(type = "Small_Integer", name = "abs", description = "Absolute value of a number")
public abstract class AbsNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  static AbsNode build() {
    return AbsNodeGen.create();
  }

  abstract Object execute(long self);

  @Specialization(rewriteOn = ArithmeticException.class)
  long doNormal(long self) {
    if (self < 0) {
      return Math.negateExact(self);
    } else {
      return self;
    }
  }

  @Specialization
  Object doOverflow(long self) {
    return toEnsoNumberNode.execute(BigIntegerOps.abs(self));
  }
}
