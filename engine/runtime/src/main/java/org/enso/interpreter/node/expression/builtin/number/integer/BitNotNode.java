package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "bit_not", description = "Bitwise negation.")
public abstract class BitNotNode extends IntegerNode {
  abstract Object execute(Object self);

  static BitNotNode build() {
    return BitNotNodeGen.create();
  }

  @Specialization
  long doLong(long self) {
    return ~self;
  }

  @Specialization
  @TruffleBoundary
  EnsoBigInteger doBigInteger(EnsoBigInteger self) {
    return new EnsoBigInteger(self.getValue().not());
  }

  @Fallback
  Object doOther(Object self) {
    throw throwTypeErrorIfNotInt(self, this);
  }
}
