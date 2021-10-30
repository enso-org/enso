package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinMethod(type = "Boolean", name = "==", description = "Computes the equality of two booleans")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBoolean(boolean _this, boolean that) {
    return _this == that;
  }

  @Specialization
  boolean doAtom(
      Atom _this,
      Atom that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef,
      @Cached("getBooleanConstructor(ctxRef)") AtomConstructor boolCons
  ) {
    var thisCons = _this.getConstructor();
    var thatCons = that.getConstructor();
    return (thatCons == boolCons) && (thisCons == thatCons);
  }

  @Fallback
  boolean doOther(Object _this, Object that) {
    return false;
  }

  AtomConstructor getBooleanConstructor(ContextReference<Context> ctxRef) {
    return ctxRef.get().getBuiltins().bool().getBool();
  }
}
