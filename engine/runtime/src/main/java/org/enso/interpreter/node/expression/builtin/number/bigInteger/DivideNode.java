package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "/", description = "Big integer division.")
public abstract class DivideNode extends Node {
  abstract double execute(EnsoBigInteger _this, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  double doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    return BigIntegerOps.toDouble(_this.getValue()) / BigIntegerOps.toDouble(that.getValue());
  }

  @Specialization
  double doLong(EnsoBigInteger _this, long that) {
    return BigIntegerOps.toDouble(_this.getValue()) / that;
  }

  @Specialization
  double doDouble(EnsoBigInteger _this, double that) {
    return BigIntegerOps.toDouble(_this.getValue()) / that;
  }

  @Fallback
  double doOther(EnsoBigInteger _this, Object that) {
    throw new TypeError("Unexpected type provided for argument `that` in Integer./", this);
  }
}
