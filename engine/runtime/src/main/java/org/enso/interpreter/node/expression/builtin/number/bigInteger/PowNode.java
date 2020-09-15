package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "^", description = "Big integer exponentiation.")
public abstract class PowNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  public abstract Object execute(EnsoBigInteger _this, Object that);

  public static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger _this, long that) {
    if (that == 0) {
      return 1L;
    } else if (that > 0) {
      return toEnsoNumberNode.execute(BigIntegerOps.pow(_this.getValue(), that));
    } else {
      return Math.pow(BigIntegerOps.toDouble(_this.getValue()), that);
    }
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    if (that.getValue().signum() > 0) {
      return Math.pow(
          BigIntegerOps.toDouble(_this.getValue()), BigIntegerOps.toDouble(that.getValue()));
    } else if (that.getValue().signum() == 0) {
      return 1.0D;
    } else {
      return 0.0D;
    }
  }

  @Specialization
  double doDouble(EnsoBigInteger _this, double that) {
    return Math.pow(BigIntegerOps.toDouble(_this.getValue()), that);
  }

  @Fallback
  Object doOther(EnsoBigInteger _this, Object that) {
    throw new TypeError("Unexpected type provided for argument `that` in Integer.^", this);
  }
}
