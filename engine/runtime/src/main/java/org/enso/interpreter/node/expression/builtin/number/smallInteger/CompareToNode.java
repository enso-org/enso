package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Small_Integer",
    name = "compare_to",
    description = "Comparison for small integers.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Object execute(long self, Object that);

  @Specialization
  Atom doLong(long self, long that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Atom doBigInt(long self, EnsoBigInteger that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self, that.getValue()));
  }

  @Specialization
  Atom doDecimal(long self, double that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Fallback
  DataflowError doOther(long self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return DataflowError.withoutTrace(typeError, this);
  }

  Ordering getOrdering() {
    return EnsoContext.get(this).getBuiltins().ordering();
  }
}
