package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinMethod(type = "Boolean", name = "==", description = "Computes the equality of two booleans")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBoolean(boolean self, boolean that) {
    return self == that;
  }

  @Specialization
  boolean doAtom(
      Atom self, Atom that, @Cached("getBooleanConstructor()") AtomConstructor boolCons) {
    var thisCons = self.getConstructor();
    var thatCons = that.getConstructor();
    return (thatCons == boolCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return false;
  }

  AtomConstructor getBooleanConstructor() {
    return Context.get(this).getBuiltins().bool().getBool();
  }
}
