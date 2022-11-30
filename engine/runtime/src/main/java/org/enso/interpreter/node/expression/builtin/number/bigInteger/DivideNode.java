package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "/", description = "Big integer division.")
public abstract class DivideNode extends Node {
  abstract double execute(EnsoBigInteger self, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
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
  double doOther(EnsoBigInteger self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
