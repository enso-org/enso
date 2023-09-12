package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
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
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@ImportStatic(BigIntegerOps.class)
@BuiltinMethod(type = "Integer", name = "bit_shift", description = "Bitwise shift.")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();
  private final CountingConditionProfile canShiftLeftInLongProfile =
      CountingConditionProfile.create();
  private final CountingConditionProfile positiveFitsInInt = CountingConditionProfile.create();
  private final CountingConditionProfile negativeFitsInInt = CountingConditionProfile.create();
  private final CountingConditionProfile rightShiftExceedsLongWidth =
      CountingConditionProfile.create();

  abstract Object execute(Object self, Object that);

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

  @Specialization(guards = {"that >= 0", "fitsInInt(that)"})
  EnsoBigInteger doBigIntShiftLeft(EnsoBigInteger self, long that) {
    return new EnsoBigInteger(BigIntegerOps.bitShiftLeft(self.getValue(), (int) that));
  }

  @Specialization(guards = "that >= 0", replaces = "doBigIntShiftLeft")
  Object doBigIntShiftLeftExplicit(
      EnsoBigInteger self,
      long that,
      @Exclusive @Cached CountingConditionProfile fitsInIntProfileLeftShift) {
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
  Object doBigIntShiftRightExplicit(
      EnsoBigInteger self,
      long that,
      @Exclusive @Cached CountingConditionProfile fitsInIntProfileRightShift) {
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
      return DataflowError.withoutTrace(
          EnsoContext.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, that, this);
  }

  boolean hasFreeBitsLeftShift(long number, long shift) {
    return shift < 64 && number > (Long.MIN_VALUE >> shift) && number < (Long.MAX_VALUE >> shift);
  }

  boolean canShiftLeftInLong(long self, long that) {
    return BigIntegerOps.fitsInInt(that) && hasFreeBitsLeftShift(self, that);
  }
}
