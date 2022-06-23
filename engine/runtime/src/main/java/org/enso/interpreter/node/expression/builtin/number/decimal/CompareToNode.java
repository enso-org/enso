package org.enso.interpreter.node.expression.builtin.number.decimal;

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

@BuiltinMethod(type = "Decimal", name = "compare_to", description = "Comparison for decimals.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Atom execute(double self, Object that);

  @Specialization
  Atom doLong(double self, long that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Atom doBigInt(double self, EnsoBigInteger that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self, that.getValue()));
  }

  @Specialization
  Atom doDecimal(double self, double that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Atom doOther(double self, Object that) {
    CompilerDirectives.transferToInterpreter();
    var number = Context.get(this).getBuiltins().number().getNumber().newInstance();
    var typeError = Context.get(this).getBuiltins().error().makeTypeError(that, number, "that");
    throw new PanicException(typeError, this);
  }

  Ordering getOrdering() {
    return Context.get(this).getBuiltins().ordering();
  }
}
