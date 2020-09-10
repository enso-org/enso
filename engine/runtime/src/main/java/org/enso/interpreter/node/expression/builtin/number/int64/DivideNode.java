package org.enso.interpreter.node.expression.builtin.number.int64;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

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
  long doBigInteger(long _this, EnsoBigInteger that) {
    return 0L;
  }

  @Fallback
  Object doOther(long _this, Object that) {
    throw new TypeError("Unexpected type provided for argument `that` in Integer./", this);
  }
}
