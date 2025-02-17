package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "negate", description = "Negation for numbers.")
public abstract class NegateNode extends IntegerNode.Unary {

  @Override
  abstract Object executeUnary(Object own);

  static NegateNode build() {
    return NegateNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doNormal(long self) {
    return Math.negateExact(self);
  }

  @Specialization
  Object doBigInt(EnsoBigInteger self) {
    return toEnsoNumberNode().execute(BigIntegerOps.negate(self.getValue()));
  }

  @Specialization
  Object doOverflow(long self) {
    return toEnsoNumberNode().execute(BigIntegerOps.negate(self));
  }

  @Fallback
  Object doOther(Object self) {
    throw throwTypeErrorIfNotInt(self, this);
  }
}
