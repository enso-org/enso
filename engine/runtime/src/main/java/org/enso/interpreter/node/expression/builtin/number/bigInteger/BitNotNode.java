package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "bit_not", description = "Bitwise negation.")
public abstract class BitNotNode extends Node {
  abstract Object execute(Object self);

  static BitNotNode build() {
    return BitNotNodeGen.create();
  }

  @Specialization
  EnsoBigInteger doLong(EnsoBigInteger self) {
    return new EnsoBigInteger(self.getValue().not());
  }

  @Fallback
  Object doOther(Object self) {
    Builtins builtins = Context.get(this).getBuiltins();
    var integer = builtins.number().getInteger();
    throw new PanicException(builtins.error().makeTypeError(integer, self, "this"), this);
  }
}
