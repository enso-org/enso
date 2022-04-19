package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "==", description = "Equality on numbers.")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doDouble(double _this, double that) {
    return _this == that;
  }

  @Specialization
  boolean doLong(double _this, long that) {
    return _this == (double) that;
  }

  @Specialization
  boolean doBigInteger(double _this, EnsoBigInteger that) {
    return _this == BigIntegerOps.toDouble(that.getValue());
  }

  @Specialization
  boolean doAtom(
      Atom _this, Atom that, @Cached("getDecimalConstructor()") AtomConstructor decimalCons) {
    var thatCons = that.getConstructor();
    var thisCons = _this.getConstructor();
    return (thatCons == decimalCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object _this, Object that) {
    return false;
  }

  AtomConstructor getDecimalConstructor() {
    return Context.get(this).getBuiltins().number().getDecimal();
  }
}
