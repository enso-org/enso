package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Ordering;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Boolean", name = "compare_to", description = "Comparison for Booleans.")
public abstract class CompareToNode extends Node {
  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Atom execute(Boolean _this, Object that);

  @Specialization
  Atom doBoolean(Boolean _this, Boolean that) {
    Ordering ordering = Context.get(this).getBuiltins().ordering();
    if (_this == that) {
      return ordering.newEqual();
    } else if (_this) {
      return ordering.newGreater();
    } else {
      return ordering.newLess();
    }
  }

  @Specialization
  Atom doOther(Boolean _this, Object that) {
    CompilerDirectives.transferToInterpreter();
    var bool = Context.get(this).getBuiltins().bool().getBool().newInstance();
    var typeError = Context.get(this).getBuiltins().error().makeTypeError(that, bool, "that");
    throw new PanicException(typeError, this);
  }
}
