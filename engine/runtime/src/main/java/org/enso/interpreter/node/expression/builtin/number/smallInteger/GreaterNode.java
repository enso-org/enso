package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = ">", description = "Comparison of numbers.")
public abstract class GreaterNode extends Node {

  abstract boolean execute(long _this, Object that);

  static GreaterNode build() {
    return GreaterNodeGen.create();
  }

  @Specialization
  boolean doLong(long _this, long that) {
    return _this > that;
  }

  @Specialization
  boolean doDouble(long _this, double that) {
    return (double) _this > that;
  }

  @Specialization
  boolean doBigInteger(long _this, EnsoBigInteger that) {
    return that.getValue().signum() < 0;
  }

  @Fallback
  boolean doOther(long _this, Object that) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    Atom number = builtins.number().getNumber().newInstance();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
