package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@ImportStatic(BigIntegerOps.class)
@BuiltinMethod(
    type = "Big_Integer",
    name = "bit_shift",
    description = "Bitwise shift.",
    aliases = "bit_shift_l")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private final ConditionProfile fitsInIntProfileLeftShift =
      ConditionProfile.createCountingProfile();
  private final ConditionProfile fitsInIntProfileRightShift =
      ConditionProfile.createCountingProfile();

  abstract Object execute(Object self, Object that);

  static BitShiftNode build() {
    return BitShiftNodeGen.create();
  }

  @Specialization(guards = {"that >= 0", "fitsInInt(that)"})
  EnsoBigInteger doBigIntShiftLeft(EnsoBigInteger self, long that) {
    return new EnsoBigInteger(BigIntegerOps.bitShiftLeft(self.getValue(), (int) that));
  }

  @Specialization(guards = "that >= 0", replaces = "doBigIntShiftLeft")
  Object doBigIntShiftLeftExplicit(EnsoBigInteger self, long that) {
    if (fitsInIntProfileLeftShift.profile(BigIntegerOps.fitsInInt(that))) {
      return doBigIntShiftLeft(self, that);
    } else {
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Specialization(guards = {"that < 0", "fitsInInt(that)"})
  Object doBigIntShiftRight(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitShiftRight(self.getValue(), (int) -that));
  }

  @Specialization(guards = "that < 0", replaces = "doBigIntShiftRight")
  Object doBigIntShiftRightExplicit(EnsoBigInteger self, long that) {
    if (fitsInIntProfileRightShift.profile(BigIntegerOps.fitsInInt(that))) {
      return doBigIntShiftRight(self, -that);
    } else {
      return BigIntegerOps.nonNegative(self.getValue()) ? 0L : -1L;
    }
  }

  @Specialization
  Object doBigIntThat(EnsoBigInteger self, EnsoBigInteger that) {
    if (!BigIntegerOps.nonNegative(that.getValue())) {
      return BigIntegerOps.nonNegative(self.getValue()) ? 0L : -1L;
    } else {
      // Note [Well-Formed BigIntegers]
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Fallback
  Object doOther(Object self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var integer = builtins.number().getInteger();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
