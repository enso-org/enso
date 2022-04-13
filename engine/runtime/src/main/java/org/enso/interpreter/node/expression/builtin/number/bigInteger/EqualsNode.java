package org.enso.interpreter.node.expression.builtin.number.bigInteger;

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

@BuiltinMethod(type = "Big_Integer", name = "==", description = "Big integer equality.")
public abstract class EqualsNode extends Node {

  abstract boolean execute(Object _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBigInt(EnsoBigInteger _this, EnsoBigInteger that) {
    return BigIntegerOps.equals(_this.getValue(), that.getValue());
  }

  @Specialization
  boolean doDouble(EnsoBigInteger _this, double that) {
    return BigIntegerOps.toDouble(_this.getValue()) == that;
  }

  @Specialization
  boolean doAtom(
      Atom _this,
      Atom that,
      @Cached("getBigIntegerConstructor()") AtomConstructor bigIntCons) {
    var thisCons = _this.getConstructor();
    var thatCons = that.getConstructor();
    return (thatCons == bigIntCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object _this, Object that) {
    return false;
  }

  AtomConstructor getBigIntegerConstructor() {
    return Context.get(this).getBuiltins().number().getBigInteger();
  }
}
