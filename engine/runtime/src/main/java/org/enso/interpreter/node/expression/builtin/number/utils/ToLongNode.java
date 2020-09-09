package org.enso.interpreter.node.expression.builtin.number.utils;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;

import java.math.BigInteger;

@ReportPolymorphism
public class ToLongNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  private static final BigInteger MIN_LONG_BIGINT = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger MAX_LONG_BIGINT = BigInteger.valueOf(Long.MAX_VALUE);

  public static ToLongNode build() {
    return new ToLongNode();
  }

  public Object execute(BigInteger bigInteger) {
    if (fitsProfile.profile(fitsInLong(bigInteger))) {
      return toLong(bigInteger);
    }
    return bigInteger;
  }

  @CompilerDirectives.TruffleBoundary
  private static boolean fitsInLong(BigInteger bigInteger) {
    return bigInteger.compareTo(MIN_LONG_BIGINT) >= 0 && bigInteger.compareTo(MAX_LONG_BIGINT) <= 0;
  }

  @CompilerDirectives.TruffleBoundary
  private static long toLong(BigInteger bigInteger) {
    return bigInteger.longValue();
  }
}
