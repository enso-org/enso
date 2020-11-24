package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;

@BuiltinMethod(type = "Small_Integer", name="bit_or", description = "Bitwise or.")
public abstract class BitOrNode extends Node {

  abstract Object execute(Object _this, Object that);

  static BitOrNode build() {
    return BitOrNodeGen.create();
  }

  @Specialization
  long doLong(long _this, long that) {
    return _this | that;
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_or", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError("Unexpected type provided for `that` in Integer.bit_or", this);
  }
}
