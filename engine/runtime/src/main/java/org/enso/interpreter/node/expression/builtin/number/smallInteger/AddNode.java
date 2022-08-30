package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "+", description = "Addition of numbers.")
public abstract class AddNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(long self, Object that);

  static AddNode build() {
    return AddNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self, long that) {
    return Math.addExact(self, that);
  }

  @Specialization
  Object doOverflow(long self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(self, that));
  }

  @Specialization
  double doDouble(long self, double that) {
    return self + that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(that.getValue(), self));
  }

  @Fallback
  Object doOther(long self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
