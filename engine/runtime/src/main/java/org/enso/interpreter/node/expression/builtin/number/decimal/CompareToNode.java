package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "compare_to", description = "Comparison for decimals.")
public abstract class CompareToNode extends Node {

  static CompareToNode build() {
    return CompareToNodeGen.create();
  }

  abstract Object execute(double self, Object that);

  @Specialization
  Object doLong(double self, long that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Specialization
  Object doBigInt(double self, EnsoBigInteger that) {
    return getOrdering().fromJava(BigIntegerOps.compareTo(self, that.getValue()));
  }

  @Specialization
  Object doDecimal(double self, double that) {
    if (self == that) {
      return getOrdering().newEqual();
    } else if (self > that) {
      return getOrdering().newGreater();
    } else {
      return getOrdering().newLess();
    }
  }

  @Fallback
  Object doOther(double self, Object that) {
    var builtins = Context.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return that == builtins.nothing() ? DataflowError.withoutTrace(typeError, this) : DataflowError.withTrace(typeError, this);
  }

  Ordering getOrdering() {
    return Context.get(this).getBuiltins().ordering();
  }
}
