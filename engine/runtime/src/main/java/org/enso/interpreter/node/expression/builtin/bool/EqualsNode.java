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
  abstract boolean execute(boolean _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBoolean(boolean _this, boolean that) {
    return _this == that;
  }

  @Specialization
  boolean doAtom(
      boolean _this,
      Atom that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef,
      @Cached("getBooleanConstructor(ctxRef)") AtomConstructor boolCons,
      @Cached("getTrueConstructor(ctxRef)") AtomConstructor trueCons,
      @Cached("getFalseConstructor(ctxRef)") AtomConstructor falseCons) {
    return (that.getConstructor() == boolCons)
        || (that.getConstructor() == trueCons)
        || (that.getConstructor() == falseCons);
  }

  @Fallback
  boolean doOther(boolean _this, Object that) {
    return false;
  }

  AtomConstructor getBooleanConstructor(ContextReference<Context> ctxRef) {
    return ctxRef.get().getBuiltins().bool().getBool();
  }

  AtomConstructor getTrueConstructor(ContextReference<Context> ctxRef) {
    return ctxRef.get().getBuiltins().bool().getTrue();
  }

  AtomConstructor getFalseConstructor(ContextReference<Context> ctxRef) {
    return ctxRef.get().getBuiltins().bool().getFalse();
  }
}
