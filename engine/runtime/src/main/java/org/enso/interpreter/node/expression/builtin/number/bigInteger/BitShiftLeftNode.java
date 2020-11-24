package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;

@BuiltinMethod(type = "Big_Integer", name = "bit_shift_l", description = "Bitwise left-shift.")
public abstract class BitShiftLeftNode extends Node {
  abstract Object execute(Object _this, Object that);

  static BitShiftLeftNode build() {
    return BitShiftLeftNodeGen.create();
  }

  @Specialization
  long doLong(long _this, long that) {
    return _this << that;
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_shift_l", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError("Unexpected type provided for `that` in Integer.bit_shift_l", this);
  }
}
