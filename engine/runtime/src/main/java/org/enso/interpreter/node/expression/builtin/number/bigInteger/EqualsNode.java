package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "==", description = "Big integer equality.")
public abstract class EqualsNode extends Node {

  abstract boolean execute(EnsoBigInteger _this, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBigInt(EnsoBigInteger _this, EnsoBigInteger that) {
    return BigIntegerOps.equals(_this.getValue(), that.getValue());
  }

  @Fallback
  boolean doOther(EnsoBigInteger _this, Object that) {
    return false;
  }
}
