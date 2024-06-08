package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;

@GenerateUncached
@GenerateInline(false)
abstract class OperationPlus extends ScalarOperationNode {
  @Override
  abstract Object executeOp(Object a, Object b) throws UnsupportedMessageException;

  static OperationPlus create() {
    return OperationPlusNodeGen.create();
  }

  static OperationPlus getUncached() {
    return OperationPlusNodeGen.getUncached();
  }

  @Specialization
  Object doLongs(long a, long b) {
    long res = a + b;
    long check1 = a ^ res;
    long check2 = b ^ res;
    long checkBoth = check1 & check2;
    if (checkBoth < 0) {
      return NullValue.get();
    }
    return res;
  }

  @Specialization(guards = {"iop.fitsInLong(a)", "iop.fitsInLong(b)"})
  Object doFitInLong(
      Object a, Object b, @Shared("iop") @CachedLibrary(limit = "3") InteropLibrary iop)
      throws UnsupportedMessageException {
    var la = iop.asLong(a);
    var lb = iop.asLong(b);
    return doLongs(la, lb);
  }

  @Specialization(guards = {"iop.isNull(a) || iop.isNull(b)"})
  NullValue nothing(
      Object a, Object b, @Shared("iop") @CachedLibrary(limit = "3") InteropLibrary iop) {
    return NullValue.get();
  }
}
