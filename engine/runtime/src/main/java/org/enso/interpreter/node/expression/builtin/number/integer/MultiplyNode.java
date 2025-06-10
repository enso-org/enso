package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "*", description = "Multiplication of numbers.")
public abstract class MultiplyNode extends IntegerNode.Binary {

  @Override
  abstract Object executeBinary(Object own, Object that);

  static MultiplyNode build() {
    return MultiplyNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self, long that) {
    return Math.multiplyExact(self, that);
  }

  @Specialization
  Object doOverflow(long self, long that) {
    return toEnsoNumberNode().execute(BigIntegerOps.multiply(self, that));
  }

  @Specialization
  double doDouble(long self, double that) {
    return ((double) self) * that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode().execute(BigIntegerOps.multiply(that.getValue(), self));
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode().execute(BigIntegerOps.multiply(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode().execute(BigIntegerOps.multiply(self.getValue(), that.getValue()));
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) * that;
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw throwTypeErrorIfNotInt(self, that);
  }
}
