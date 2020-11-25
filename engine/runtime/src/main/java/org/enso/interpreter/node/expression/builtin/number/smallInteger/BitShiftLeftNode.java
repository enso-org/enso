package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "bit_shift_l", description = "Bitwise left-shift.")
public abstract class BitShiftLeftNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(Object _this, Object that);

  static BitShiftLeftNode build() {
    return BitShiftLeftNodeGen.create();
  }

  // TODO [AA] Handle negative shift values by delegating to the other nodes

  @Specialization(guards = {"fitsInInt(that)", "hasFreeBits(_this, that)", "that > 0"})
  Object doLong(long _this, long that) {
    return _this;
  }

  @Specialization(replaces = "doLong")
  Object doLongExplicit(long _this, long that) {
    // has an explicit if for `fitsInInt` with profile.
    return _this;
  }

  @Specialization
  Object doBigInteger(
      long _this,
      EnsoBigInteger that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    CompilerDirectives.transferToInterpreter();
    throw new PanicException(
        ctxRef.get().getBuiltins().error().getShiftAmountTooLargeError(), this);
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_shift_l", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError("Unexpected type provided for `that` in Integer.bit_shift_l", this);
  }

  boolean hasFreeBits(long number, long shift) {
    var lastBit = number >> 63;
    return false;
  }

  boolean fitsInInt(long number) {
    return ((int) number) == number;
  }

  boolean isPositiveShift(long number) {
    return number >= 0;
  }
}
