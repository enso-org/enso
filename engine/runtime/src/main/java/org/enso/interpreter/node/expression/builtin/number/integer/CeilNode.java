package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "ceil", description = "Small integer ceiling.")
public abstract class CeilNode extends Node {
  abstract Object execute(Object self);

  public static CeilNode build() {
    return CeilNodeGen.create();
  }

  @Specialization
  long doLong(long self) {
    return self;
  }

  @Specialization
  EnsoBigInteger doBigInt(EnsoBigInteger self) {
    return self;
  }

  @Fallback
  Object doOther(Object self) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, this);
  }
}
