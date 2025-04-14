package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Float",
    name = "ceil",
    description = "Float ceiling, converting to an integer.")
public abstract class CeilNode extends FloatNode {
  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  static CeilNode build() {
    return CeilNodeGen.create();
  }

  abstract Object execute(double own);

  @Specialization
  Object doDouble(double own, @CachedLibrary(limit = "1") InteropLibrary interop) {
    double ceil = Math.ceil(own);
    if (fitsProfile.profile(interop.fitsInLong(ceil))) {
      return (long) ceil;
    } else {
      return new EnsoBigInteger(ceil(ceil));
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static BigInteger ceil(double ceil) {
    return BigDecimal.valueOf(ceil).toBigIntegerExact();
  }
}
