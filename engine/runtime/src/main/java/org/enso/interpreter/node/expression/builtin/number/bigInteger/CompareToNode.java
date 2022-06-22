package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Ordering;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Big_Integer",
    name = "compare_to",
    description = "Comparison for big integers.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Atom execute(EnsoBigInteger self, Object that);

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

  @Specialization
  Atom doOther(EnsoBigInteger self, Object that) {
    CompilerDirectives.transferToInterpreter();
    var number = Context.get(this).getBuiltins().number().getNumber().newInstance();
    var typeError = Context.get(this).getBuiltins().error().makeTypeError(that, number, "that");
    throw new PanicException(typeError, this);
  }

  Ordering getOrdering() {
    return Context.get(this).getBuiltins().ordering();
  }
}
