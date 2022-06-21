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

  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBigInt(EnsoBigInteger self, EnsoBigInteger that) {
    return BigIntegerOps.equals(self.getValue(), that.getValue());
  }

  @Specialization
  boolean doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) == that;
  }

  @Specialization
  boolean doAtom(
      Atom self, Atom that, @Cached("getBigIntegerConstructor()") AtomConstructor bigIntCons) {
    var thisCons = self.getConstructor();
    var thatCons = that.getConstructor();
    return (thatCons == bigIntCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return false;
  }

  AtomConstructor getBigIntegerConstructor() {
    return Context.get(this).getBuiltins().number().getBigInteger();
  }
}
