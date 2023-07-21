package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@ImportStatic(BigIntegerOps.class)
@BuiltinMethod(type = "Small_Integer", name = "bit_shift", description = "Bitwise shift.")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private final CountingConditionProfile canShiftLeftInLongProfile =
      CountingConditionProfile.create();
  private final CountingConditionProfile positiveFitsInInt = CountingConditionProfile.create();
  private final CountingConditionProfile negativeFitsInInt = CountingConditionProfile.create();
  private final CountingConditionProfile rightShiftExceedsLongWidth =
      CountingConditionProfile.create();

  abstract Object execute(long self, Object that);

  @NeverDefault
  static BitShiftNode build() {
    return BitShiftNodeGen.create();
  }

  @Specialization(guards = {"that >= 0", "canShiftLeftInLong(self, that)"})
  long doLongShiftLeft(long self, long that) {
    return self << that;
  }

  @Specialization(guards = "that >= 0", replaces = "doLongShiftLeft")
  Object doLongShiftLeftExplicit(long self, long that) {
    if (canShiftLeftInLongProfile.profile(canShiftLeftInLong(self, that))) {
      return doLongShiftLeft(self, that);
    } else if (positiveFitsInInt.profile(BigIntegerOps.fitsInInt(that))) {
      return toEnsoNumberNode.execute(BigIntegerOps.bitShiftLeft(self, (int) that));
    } else {
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Specialization(guards = {"that < 0", "fitsInInt(that)"})
  long doLongShiftRight(long self, long that) {
    if (rightShiftExceedsLongWidth.profile(-that < 64)) {
      return self >> -that;
    } else {
      return self >= 0 ? 0L : -1L;
    }
  }

  @Specialization(guards = "that < 0", replaces = "doLongShiftRight")
  long doLongShiftRightExplicit(long self, long that) {
    if (negativeFitsInInt.profile(BigIntegerOps.fitsInInt(that))) {
      return doLongShiftRight(self, that);
    } else {
      return self >= 0 ? 0L : -1L;
    }
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    if (!BigIntegerOps.nonNegative(that.getValue())) {
      return self >= 0 ? 0L : -1L;
    } else {
      // Note [Well-Formed BigIntegers]
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Fallback
  Object doOther(long self, Object that) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    var integer = builtins.number().getInteger();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }

  boolean hasFreeBitsLeftShift(long number, long shift) {
    return shift < 64 && number > (Long.MIN_VALUE >> shift) && number < (Long.MAX_VALUE >> shift);
  }

  boolean canShiftLeftInLong(long self, long that) {
    return BigIntegerOps.fitsInInt(that) && hasFreeBitsLeftShift(self, that);
  }
}
