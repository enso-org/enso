package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(type = "Boolean", name = "compare_to", description = "Comparison for Booleans.")
public abstract class CompareToNode extends Node {
  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Object execute(Boolean self, Object that);

  @Specialization
  Atom doBoolean(Boolean self, Boolean that) {
    Ordering ordering = EnsoContext.get(this).getBuiltins().ordering();
    if (self == that) {
      return ordering.newEqual();
    } else if (self) {
      return ordering.newGreater();
    } else {
      return ordering.newLess();
    }
  }

  @Fallback
  DataflowError doOther(Boolean self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.bool().getType(), that, "that");
    return DataflowError.withoutTrace(typeError, this);
  }
}
