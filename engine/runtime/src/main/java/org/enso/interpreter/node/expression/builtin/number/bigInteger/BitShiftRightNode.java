package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "bit_shift_r", description = "Bitwise right-shift.")
public abstract class BitShiftRightNode extends Node {
  private @Child BitShiftNode bitShiftNode = BitShiftNode.build();

  abstract Object execute(Object _this, Object that);

  static BitShiftRightNode build() {
    return BitShiftRightNodeGen.create();
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, long that) {
    return bitShiftNode.execute(_this, -1L * that);
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    // Note [No Negation]
    return bitShiftNode.execute(_this, that);
  }

  /* Note [No Negation]
   * ~~~~~~~~~~~~~~~~~~
   * As having an `EnsoBigInteger` value as the shift size is always ill-formed, we need not bother
   * with flipping the sign here.
   */

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_shift_r", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError("Unexpected type provided for `that` in Integer.bit_shift_r", this);
  }
}
