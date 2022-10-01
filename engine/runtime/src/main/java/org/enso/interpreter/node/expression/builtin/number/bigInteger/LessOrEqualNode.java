package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "<=", description = "Comparison of numbers.")
public abstract class LessOrEqualNode extends Node {

  abstract boolean execute(EnsoBigInteger self, Object that);

  static LessOrEqualNode build() {
    return LessOrEqualNodeGen.create();
  }

  @Specialization
  boolean doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) <= that;
  }

  @Specialization
  boolean doLong(EnsoBigInteger self, long that) {
    return self.getValue().signum() < 0;
  }

  @Specialization
  boolean doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return BigIntegerOps.compare(self.getValue(), that.getValue()) <= 0;
  }

  @Fallback
  boolean doOther(EnsoBigInteger self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
