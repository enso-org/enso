package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "^", description = "Big integer exponentiation.")
public abstract class PowNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  public abstract Object execute(EnsoBigInteger self, Object that);

  public static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    if (that == 0) {
      return 1L;
    } else if (that > 0) {
      return toEnsoNumberNode.execute(BigIntegerOps.pow(self.getValue(), that));
    } else {
      return Math.pow(BigIntegerOps.toDouble(self.getValue()), that);
    }
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    if (that.getValue().signum() > 0) {
      return Math.pow(
          BigIntegerOps.toDouble(self.getValue()), BigIntegerOps.toDouble(that.getValue()));
    } else if (that.getValue().signum() == 0) {
      return 1.0D;
    } else {
      return 0.0D;
    }
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return Math.pow(BigIntegerOps.toDouble(self.getValue()), that);
  }

  @Fallback
  Object doOther(EnsoBigInteger self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
