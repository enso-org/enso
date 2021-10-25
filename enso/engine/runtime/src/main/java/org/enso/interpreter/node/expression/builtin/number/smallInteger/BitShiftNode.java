package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@ImportStatic(BigIntegerOps.class)
@BuiltinMethod(type = "Small_Integer", name = "bit_shift", description = "Bitwise shift.")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private final ConditionProfile canShiftLeftInLongProfile =
      ConditionProfile.createCountingProfile();
  private final ConditionProfile positiveFitsInInt = ConditionProfile.createCountingProfile();
  private final ConditionProfile negativeFitsInInt = ConditionProfile.createCountingProfile();
  private final ConditionProfile rightShiftExceedsLongWidth =
      ConditionProfile.createCountingProfile();

  abstract Object execute(Object _this, Object that);

  static BitShiftNode build() {
    return BitShiftNodeGen.create();
  }

  @Specialization(guards = {"that >= 0", "canShiftLeftInLong(_this, that)"})
  long doLongShiftLeft(long _this, long that) {
    return _this << that;
  }

  @Specialization(guards = "that >= 0", replaces = "doLongShiftLeft")
  Object doLongShiftLeftExplicit(
      long _this, long that, @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    if (canShiftLeftInLongProfile.profile(canShiftLeftInLong(_this, that))) {
      return doLongShiftLeft(_this, that);
    } else if (positiveFitsInInt.profile(BigIntegerOps.fitsInInt(that))) {
      return toEnsoNumberNode.execute(BigIntegerOps.bitShiftLeft(_this, (int) that));
    } else {
      return DataflowError.withoutTrace(
          ctxRef.get().getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Specialization(guards = {"that < 0", "fitsInInt(that)"})
  long doLongShiftRight(long _this, long that) {
    if (rightShiftExceedsLongWidth.profile(-that < 64)) {
      return _this >> -that;
    } else {
      return _this >= 0 ? 0L : -1L;
    }
  }

  @Specialization(guards = "that < 0", replaces = "doLongShiftRight")
  long doLongShiftRightExplicit(long _this, long that) {
    if (negativeFitsInInt.profile(BigIntegerOps.fitsInInt(that))) {
      return doLongShiftRight(_this, that);
    } else {
      return _this >= 0 ? 0L : -1L;
    }
  }

  @Specialization
  Object doBigInteger(
      long _this,
      EnsoBigInteger that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    if (!BigIntegerOps.nonNegative(that.getValue())) {
      return _this >= 0 ? 0L : -1L;
    } else {
      // Note [Well-Formed BigIntegers]
      return DataflowError.withoutTrace(
          ctxRef.get().getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  /* Note [Well-Formed BigIntegers]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * As EnsoBigInteger should only be encountered when the integral value cannot be represented in
   * a standard `long`, we can safely rule out that the shift width is zero.
   */

  @Specialization
  Object doAtomThis(Atom _this, Object that, @CachedContext(Language.class) Context ctx) {
    Builtins builtins = ctx.getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "this"), this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }

  boolean hasFreeBitsLeftShift(long number, long shift) {
    return shift < 64 && number > (Long.MIN_VALUE >> shift) && number < (Long.MAX_VALUE >> shift);
  }

  boolean canShiftLeftInLong(long _this, long that) {
    return BigIntegerOps.fitsInInt(that) && hasFreeBitsLeftShift(_this, that);
  }
}
