package org.enso.interpreter.node.expression.builtin.number.int64;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToLongNode;

import java.math.BigInteger;

@BuiltinMethod(type = "Number", name = "*", description = "Multiplication of numbers.")
public abstract class MultiplyNode extends Node {
  private @Child ToLongNode toLongNode = ToLongNode.build();

  abstract Object execute(long _this, Object that);

  static MultiplyNode build() {
    return MultiplyNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long _this, long that) {
    return Math.multiplyExact(_this, that);
  }

  @Specialization
  Object doOverflow(long _this, long that) {
    return toLongNode.execute(BigIntegerOps.multiply(_this, that));
  }

  @Specialization
  Object doBigInteger(long _this, BigInteger that) {
    return toLongNode.execute(BigIntegerOps.multiply(that, _this));
  }
}
