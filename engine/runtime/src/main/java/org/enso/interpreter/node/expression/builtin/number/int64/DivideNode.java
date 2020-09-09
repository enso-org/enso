package org.enso.interpreter.node.expression.builtin.number.int64;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

import java.math.BigInteger;

@BuiltinMethod(type = "Int_64", name = "/", description = "Division of numbers.")
public abstract class DivideNode extends Node {
  abstract Object execute(long _this, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  long doLong(long _this, long that) {
    return _this / that;
  }

  @Specialization
  long doBigInteger(long _this, BigInteger that) {
    return 0L;
  }
}
