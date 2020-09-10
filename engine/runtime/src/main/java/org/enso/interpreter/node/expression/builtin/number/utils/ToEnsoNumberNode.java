package org.enso.interpreter.node.expression.builtin.number.utils;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

import java.math.BigInteger;

@ReportPolymorphism
@NodeInfo(description = "Takes a big integer and casts it to a long, if the operation is safe.")
public class ToEnsoNumberNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  private static final BigInteger MIN_LONG_BIGINT = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger MAX_LONG_BIGINT = BigInteger.valueOf(Long.MAX_VALUE);

  /** @return a new instance of this node. */
  public static ToEnsoNumberNode build() {
    return new ToEnsoNumberNode();
  }

  /**
   * Execute the node.
   *
   * @param bigInteger the big integer to downcast.
   * @return a long with the same value if possible, the original integer wrapped in {@link
   *     org.enso.interpreter.runtime.number.EnsoBigInteger} otherwise.
   */
  public Object execute(BigInteger bigInteger) {
    if (fitsProfile.profile(fitsInLong(bigInteger))) {
      return toLong(bigInteger);
    }
    return new EnsoBigInteger(bigInteger);
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
