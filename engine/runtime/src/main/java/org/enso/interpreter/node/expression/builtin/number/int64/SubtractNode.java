package org.enso.interpreter.node.expression.builtin.number.int64;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Int_64", name = "-", description = "Subtraction of numbers.")
public abstract class SubtractNode extends Node {
  private @Child ToEnsoNumberNode toLongNode = ToEnsoNumberNode.build();

  abstract Object execute(long _this, Object that);

  static SubtractNode build() {
    return SubtractNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long _this, long that) {
    return Math.subtractExact(_this, that);
  }

  @Specialization
  Object doOverflow(long _this, long that) {
    return toLongNode.execute(BigIntegerOps.subtract(_this, that));
  }

  @Specialization
  Object doBigInteger(long _this, EnsoBigInteger that) {
    return toLongNode.execute(BigIntegerOps.subtract(_this, that.getValue()));
  }
}
