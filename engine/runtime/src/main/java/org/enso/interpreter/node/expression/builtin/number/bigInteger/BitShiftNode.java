package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.CompilerDirectives;
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
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@ImportStatic(BigIntegerOps.class)
@BuiltinMethod(type = "Big_Integer", name = "bit_shift", description = "Bitwise shift.")
public abstract class BitShiftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private final ConditionProfile shiftPositive = ConditionProfile.createCountingProfile();

  abstract Object execute(Object _this, Object that);

  static BitShiftNode build() {
    return BitShiftNodeGen.create();
  }

  @Specialization(guards = {"fitsInInt(that)", "that >= 0"})
  EnsoBigInteger doBigIntegerShiftLeft(EnsoBigInteger _this, long that) {
    return new EnsoBigInteger(BigIntegerOps.bitShiftLeft(_this.getValue(), (int) that));
  }

  @Specialization(guards = {"fitsInInt(that)", "that < 0"})
  Object doBigIntegerShiftRight(EnsoBigInteger _this, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitShiftRight(_this.getValue(), (int) that));
  }

  @Specialization
  Object doBigIntegerThat(
      EnsoBigInteger _this,
      EnsoBigInteger that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    CompilerDirectives.transferToInterpreter();
    throw new PanicException(
        ctxRef.get().getBuiltins().error().getShiftAmountTooLargeError(), this);
  }

  @Specialization(replaces = {"doBigIntegerShiftLeft", "doBigIntegerShiftRight"})
  Object doBigIntegerExplicit(
      EnsoBigInteger _this,
      long that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    if (!BigIntegerOps.fitsInInt(that)) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          ctxRef.get().getBuiltins().error().getShiftAmountTooLargeError(), this);
    } else {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException("Code should not be reachable.");
    }
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
