package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "+", description = "Big integer addition.")
public abstract class AddNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();

  abstract Object execute(EnsoBigInteger self, Object that);

  static AddNode build() {
    return AddNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(self.getValue(), that.getValue()));
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) + that;
  }

  @Fallback
  Object doOther(EnsoBigInteger self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
