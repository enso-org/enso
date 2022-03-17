package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "/", description = "Division of numbers.")
public abstract class DivideNode extends Node {
  abstract double execute(long _this, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  double doLong(long _this, long that) {
    return ((double) _this) / ((double) that);
  }

  @Specialization
  double doDouble(long _this, double that) {
    return _this / that;
  }

  @Specialization
  double doBigInteger(long _this, EnsoBigInteger that) {
    return ((double) _this) / BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  double doOther(long _this, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom number = builtins.number().getNumber().newInstance();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
