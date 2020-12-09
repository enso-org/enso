package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "bit_not", description = "Bitwise negation.")
public abstract class BitNotNode extends Node {
  abstract Object execute(Object _this);

  static BitNotNode build() {
    return BitNotNodeGen.create();
  }

  @Specialization
  EnsoBigInteger doLong(EnsoBigInteger _this) {
    return new EnsoBigInteger(_this.getValue().not());
  }

  @Fallback
  Object doOther(Object _this) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_not", this);
  }
}
