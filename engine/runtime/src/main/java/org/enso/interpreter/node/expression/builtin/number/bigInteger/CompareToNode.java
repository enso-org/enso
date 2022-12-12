package org.enso.interpreter.node.expression.builtin.number.bigInteger;

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
    type = "Big_Integer",
    name = "compare_to",
    description = "Comparison for big integers.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Object execute(EnsoBigInteger self, Object that);

  @Specialization
  Atom doLong(EnsoBigInteger self, long that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self.getValue(), that));
  }

  @Specialization
  Atom doBigInt(EnsoBigInteger self, EnsoBigInteger that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self.getValue(), that.getValue()));
  }

  @Specialization
  Atom doDecimal(EnsoBigInteger self, double that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self.getValue(), that));
  }

  @Fallback
  DataflowError doOther(EnsoBigInteger self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return DataflowError.withoutTrace(typeError, this);
  }

  Ordering getOrdering() {
    return EnsoContext.get(this).getBuiltins().ordering();
  }
}
