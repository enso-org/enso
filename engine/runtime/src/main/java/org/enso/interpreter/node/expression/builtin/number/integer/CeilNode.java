package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "ceil", description = "Small integer ceiling.")
public abstract class CeilNode extends IntegerNode {
  abstract Object execute(Object own);

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
    throw throwTypeErrorIfNotInt(self, this);
  }
}
