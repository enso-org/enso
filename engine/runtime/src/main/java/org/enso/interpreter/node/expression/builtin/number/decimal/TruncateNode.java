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
    name = "truncate_builtin",
    description = "Truncate a floating-point number to an integer by dropping the fractional part.")
public abstract class TruncateNode extends FloatNode {
  static TruncateNode build() {
    return TruncateNodeGen.create();
  }

  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  abstract Object execute(double own);

  @Specialization(limit = "1")
  Object doDouble(double own, @CachedLibrary(value = "own") InteropLibrary interop) {
    if (fitsProfile.profile(interop.fitsInLong(own))) {
      return (long) own;
    } else {
      return new EnsoBigInteger(toBigInteger(own));
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static BigInteger toBigInteger(double self) {
    return BigDecimal.valueOf(self).toBigIntegerExact();
  }
}
