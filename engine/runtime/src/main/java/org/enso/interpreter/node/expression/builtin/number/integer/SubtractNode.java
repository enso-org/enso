package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "-", description = "Subtraction of numbers.")
public abstract class SubtractNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();

  abstract Object execute(Object self, Object that);

  static SubtractNode build() {
    return SubtractNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self, long that) {
    return Math.subtractExact(self, that);
  }

  @Specialization
  Object doOverflow(long self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self, that));
  }

  @Specialization
  double doDouble(long self, double that) {
    return self - that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self, that.getValue()));
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self.getValue(), that.getValue()));
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) - that;
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, that, this);
  }
}
