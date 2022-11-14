package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(type = "Boolean", name = "compare_to", description = "Comparison for Booleans.")
public abstract class CompareToNode extends Node {
  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Object execute(Boolean self, Object that);

  @Specialization
  Object doBoolean(Boolean self, Boolean that) {
    Ordering ordering = Context.get(this).getBuiltins().ordering();
    if (self == that) {
      return ordering.newEqual();
    } else if (self) {
      return ordering.newGreater();
    } else {
      return ordering.newLess();
    }
  }

  @Specialization
  Object doOther(double self, Object that) {
    CompilerDirectives.transferToInterpreter();
    var number = Context.get(this).getBuiltins().number().getNumber();
    var typeError = Context.get(this).getBuiltins().error().makeTypeError(that, number, "that");
    return DataflowError.withoutTrace(typeError, this);
  }
}
