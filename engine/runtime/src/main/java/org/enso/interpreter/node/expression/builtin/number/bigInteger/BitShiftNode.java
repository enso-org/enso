package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
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
@BuiltinMethod(type = "Big_Integer", name = "bit_shift", description = "Bitwise shift.")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private final ConditionProfile fitsInIntProfileLeftShift =
      ConditionProfile.createCountingProfile();
  private final ConditionProfile fitsInIntProfileRightShift =
      ConditionProfile.createCountingProfile();

  abstract Object execute(Object _this, Object that);

  static BitShiftNode build() {
    return BitShiftNodeGen.create();
  }

  @Specialization(guards = {"that >= 0", "fitsInInt(that)"})
  EnsoBigInteger doBigIntShiftLeft(EnsoBigInteger _this, long that) {
    return new EnsoBigInteger(BigIntegerOps.bitShiftLeft(_this.getValue(), (int) that));
  }

  @Specialization(guards = "that >= 0", replaces = "doBigIntShiftLeft")
  Object doBigIntShiftLeftExplicit(EnsoBigInteger _this, long that) {
    if (fitsInIntProfileLeftShift.profile(BigIntegerOps.fitsInInt(that))) {
      return doBigIntShiftLeft(_this, that);
    } else {
      return DataflowError.withoutTrace(
          Context.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Specialization(guards = {"that < 0", "fitsInInt(that)"})
  Object doBigIntShiftRight(EnsoBigInteger _this, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitShiftRight(_this.getValue(), (int) -that));
  }

  @Specialization(guards = "that < 0", replaces = "doBigIntShiftRight")
  Object doBigIntShiftRightExplicit(EnsoBigInteger _this, long that) {
    if (fitsInIntProfileRightShift.profile(BigIntegerOps.fitsInInt(that))) {
      return doBigIntShiftRight(_this, -that);
    } else {
      return BigIntegerOps.nonNegative(_this.getValue()) ? 0L : -1L;
    }
  }

  @Specialization
  Object doBigIntThat(EnsoBigInteger _this, EnsoBigInteger that) {
    if (!BigIntegerOps.nonNegative(that.getValue())) {
      return BigIntegerOps.nonNegative(_this.getValue()) ? 0L : -1L;
    } else {
      // Note [Well-Formed BigIntegers]
      return DataflowError.withoutTrace(
          Context.get(this).getBuiltins().error().getShiftAmountTooLargeError(), this);
    }
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, _this, "this"), this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
