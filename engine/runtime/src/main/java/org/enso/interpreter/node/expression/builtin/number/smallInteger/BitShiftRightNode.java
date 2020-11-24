package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;

@BuiltinMethod(
    type = "Small_Integer",
    name = "bit_shift_r",
    description = "Bitwise right-shift.")
public abstract class BitShiftRightNode extends Node {
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  abstract Object execute(Object _this, Object that);

  static BitShiftRightNode build() {
    return BitShiftRightNodeGen.create();
  }

  @Specialization
  long doLong(long _this, long that) {
    return _this >> that;
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError(
        "Unexpected type provided for `this` in Integer.bit_shift_r", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError(
        "Unexpected types provided for that in Integer.bit_shift_r", this);
  }
}
