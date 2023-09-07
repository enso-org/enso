package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "/", description = "Division of numbers.")
public abstract class DivideNode extends Node {
  abstract double execute(Object self, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  double doLong(long self, long that) {
    return ((double) self) / ((double) that);
  }

  @Specialization
  double doDouble(long self, double that) {
    return self / that;
  }

  @Specialization
  double doBigInteger(long self, EnsoBigInteger that) {
    return ((double) self) / BigIntegerOps.toDouble(that.getValue());
  }

  @Specialization
  double doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return BigIntegerOps.toDouble(self.getValue()) / BigIntegerOps.toDouble(that.getValue());
  }

  @Specialization
  double doLong(EnsoBigInteger self, long that) {
    return BigIntegerOps.toDouble(self.getValue()) / that;
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) / that;
  }

  @Fallback
  double doOther(Object self, Object that) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, that, this);
  }
}
