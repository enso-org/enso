package org.enso.interpreter.node.expression.builtin.number.smallInteger;

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
    type = "Small_Integer",
    name = "compare_to",
    description = "Comparison for small integers.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Atom execute(long _this, Object that);

  @Specialization
  Atom doLong(
      long _this,
      long that) {
    if (_this == that) {
      return getOrdering().newEqual();
    } else if (_this > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Atom doBigInt(
      long _this,
      EnsoBigInteger that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(_this, that.getValue()));
  }

  @Specialization
  Atom doDecimal(
      long _this,
      double that) {
    if (_this == that) {
      return getOrdering().newEqual();
    } else if (_this > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Atom doOther(
      long _this, Object that) {
    CompilerDirectives.transferToInterpreter();
    var number = Context.get(this).getBuiltins().number().getNumber().newInstance();
    var typeError = Context.get(this).getBuiltins().error().makeTypeError(that, number, "that");
    throw new PanicException(typeError, this);
  }

  Ordering getOrdering() {
    return Context.get(this).getBuiltins().ordering();
  }
}
