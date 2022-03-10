package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinMethod(type = "Small_Integer", name = "==", description = "Equality on numbers.")
public abstract class EqualsNode extends Node {

  abstract boolean execute(Object _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doLong(long _this, long that) {
    return _this == that;
  }

  @Specialization
  boolean doDouble(long _this, double that) {
    return (double) _this == that;
  }

  @Specialization
  boolean doAtom(
      Atom _this,
      Atom that,
      @Cached("getSmallIntegerConstructor()") AtomConstructor smallIntCons) {
    var thisCons = _this.getConstructor();
    var thatCons = that.getConstructor();
    return (thatCons == smallIntCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object _this, Object that) {
    return false;
  }

  AtomConstructor getSmallIntegerConstructor() {
    return Context.get(this).getBuiltins().number().getBigInteger();
  }
}
