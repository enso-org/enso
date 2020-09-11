package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "^", description = "Exponentiation of numbers.")
public abstract class PowNode extends Node {
  abstract double execute(double _this, Object that);

  static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  double doDouble(double _this, double that) {
    return Math.pow(_this, that);
  }

  @Specialization
  double doLong(double _this, long that) {
    return Math.pow(_this, that);
  }

  @Specialization
  double doBigInteger(double _this, EnsoBigInteger that) {
    return Math.pow(_this, BigIntegerOps.toDouble(that.getValue()));
  }

  @Fallback
  double doOther(double _this, Object that) {
    throw new TypeError("Unexpected type provided for argument `that` in Decimal.^", this);
  }
}
