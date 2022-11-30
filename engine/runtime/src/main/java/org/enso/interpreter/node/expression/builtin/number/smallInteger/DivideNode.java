package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "/", description = "Division of numbers.")
public abstract class DivideNode extends Node {
  abstract double execute(long self, Object that);

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

  @Fallback
  double doOther(long self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
