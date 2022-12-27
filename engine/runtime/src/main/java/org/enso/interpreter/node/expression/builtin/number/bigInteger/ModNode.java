package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "%", description = "Big integer modulo division.")
public abstract class ModNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(EnsoBigInteger self, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    try {
      return toEnsoNumberNode.execute(BigIntegerOps.modulo(self.getValue(), that));
    } catch (ArithmeticException e) {
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getDivideByZeroError(), this);
    }
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    // No need to trap, as floating-point modulo returns NaN for division by zero instead of
    // throwing.
    return BigIntegerOps.toDouble(self.getValue()) % that;
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    // No need to trap, as 0 is never represented as an EnsoBigInteger.
    return toEnsoNumberNode.execute(BigIntegerOps.modulo(self.getValue(), that.getValue()));
  }

  @Fallback
  Object doOther(EnsoBigInteger self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
