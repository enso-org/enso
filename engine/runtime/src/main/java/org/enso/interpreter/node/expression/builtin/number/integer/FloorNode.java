package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "floor", description = "Small integer floor.")
public abstract class FloorNode extends IntegerNode {

  public abstract Object execute(Object self);

  public static FloorNode build() {
    return FloorNodeGen.create();
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
