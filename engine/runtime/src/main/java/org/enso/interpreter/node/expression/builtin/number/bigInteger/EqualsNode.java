package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

import java.math.BigInteger;

@BuiltinMethod(type = "Big_Integer", name = "==", description = "Equality on numbers.")
public abstract class EqualsNode extends Node {

  abstract boolean execute(BigInteger _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doLong(BigInteger _this, BigInteger that) {
    return _this.equals(that);
  }

  @Fallback
  boolean doOther(BigInteger _this, Object that) {
    return false;
  }
}
