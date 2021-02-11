package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Small_Integer", name = "bit_not", description = "Bitwise negation.")
public abstract class BitNotNode extends Node {
  abstract Object execute(Object _this);

  static BitNotNode build() {
    return BitNotNodeGen.create();
  }

  @Specialization
  long doLong(long _this) {
    return ~_this;
  }

  @Fallback
  Object doOther(Object _this) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, _this, "this"), this);
  }
}
