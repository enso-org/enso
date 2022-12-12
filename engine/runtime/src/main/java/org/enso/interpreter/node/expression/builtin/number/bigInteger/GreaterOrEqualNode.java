package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = ">=", description = "Comparison of numbers.")
public abstract class GreaterOrEqualNode extends Node {

  abstract Object execute(EnsoBigInteger self, Object that);

  static GreaterOrEqualNode build() {
    return GreaterOrEqualNodeGen.create();
  }

  @Specialization
  boolean doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) >= that;
  }

  @Specialization
  boolean doLong(EnsoBigInteger self, long that) {
    return self.getValue().signum() > 0;
  }

  @Specialization
  boolean doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return BigIntegerOps.compare(self.getValue(), that.getValue()) >= 0;
  }

  @Fallback
  DataflowError doOther(EnsoBigInteger self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return DataflowError.withoutTrace(typeError, this);
  }
}
