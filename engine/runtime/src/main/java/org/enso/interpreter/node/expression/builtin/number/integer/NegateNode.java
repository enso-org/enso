package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "negate", description = "Negation for numbers.")
public abstract class NegateNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();

  static NegateNode build() {
    return NegateNodeGen.create();
  }

  abstract Object execute(Object self);

  @Specialization(rewriteOn = ArithmeticException.class)
  long doNormal(long self) {
    return Math.negateExact(self);
  }

  @Specialization
  Object doBigInt(EnsoBigInteger self) {
    return toEnsoNumberNode.execute(BigIntegerOps.negate(self.getValue()));
  }

  @Specialization
  Object doOverflow(long self) {
    return toEnsoNumberNode.execute(BigIntegerOps.negate(self));
  }

  @Fallback
  Object doOther(Object self) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, this);
  }
}
