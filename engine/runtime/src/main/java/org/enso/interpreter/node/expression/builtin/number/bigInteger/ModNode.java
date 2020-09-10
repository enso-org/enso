package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "%", description = "Big integer modulo division.")
public abstract class ModNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(EnsoBigInteger _this, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger _this, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.modulo(_this.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.modulo(_this.getValue(), that.getValue()));
  }

  @Fallback
  Object doOther(EnsoBigInteger _this, Object that) {
    throw new TypeError("Unexpected type provided for argument `that` in Integer.%", this);
  }
}
