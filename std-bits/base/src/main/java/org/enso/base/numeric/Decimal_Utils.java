package org.enso.base.numeric;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

/** Utils for the Enso Decmial type. */
public class Decimal_Utils {
  private static final BigDecimal MIN_LONG_BIGDECIMAL = BigDecimal.valueOf(Long.MIN_VALUE);
  private static final BigDecimal MAX_LONG_BIGDECIMAL = BigDecimal.valueOf(Long.MAX_VALUE);

  public static BigDecimal fromString(String s) {
    return new BigDecimal(s);
  }

  public static ConversionResult fromString(String s, MathContext mc) {
    BigDecimal bd = new BigDecimal(s, mc);
    BigDecimal withoutMC = new BigDecimal(s);
    return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
  }

  public static BigDecimal fromInteger(Object o) {
    if (o instanceof Long l) {
      // According to the BigInteger Javadocs, valueOf is preferred "because it
      // allows for reuse of frequently used BigDecimal values".
      return BigDecimal.valueOf(l);
    } else if (o instanceof BigInteger bi) {
      return new BigDecimal(bi);
    } else {
      throw new IllegalArgumentException("Input must be Long or BigInteger");
    }
  }

  public static ConversionResult fromInteger(Object o, MathContext mc) {
    switch (o) {
      case Long l -> {
        BigDecimal bd = new BigDecimal(l, mc);
        BigDecimal withoutMC = new BigDecimal(l);
        long backToLong = bd.longValue();
        return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
      }
      case BigInteger bi -> {
        BigDecimal bd = new BigDecimal(bi, mc);
        BigDecimal withoutMC = new BigDecimal(bi);
        BigInteger backToBigInteger = bd.toBigInteger();
        return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
      }
      case null, default -> {
        throw new IllegalArgumentException("Input must be Long or BigInteger");
      }
    }
  }

  public static BigDecimal fromFloat(Double d) {
    // According to the BigInteger Javadocs, valueOf is preferred because "the
    // value returned is equal to that resulting from constructing a BigDecimal
    // from the result of using Double.toString(double)."
    return BigDecimal.valueOf(d);
  }

  public static BigDecimal fromFloat(Double d, MathContext mc) {
    // We do not check for precision loss here because we always attach a
    // warning when converting from float.
    return new BigDecimal(d, mc);
  }

  public static boolean fitsInLong(BigDecimal bigDecimal) {
    return bigDecimal.compareTo(MIN_LONG_BIGDECIMAL) >= 0
        && bigDecimal.compareTo(MAX_LONG_BIGDECIMAL) <= 0;
  }

  public static int hashCodeOf(BigDecimal bd) {
    boolean isFractional = bd.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) != 0;
    boolean fitsInLong = fitsInLong(bd);
    if (isFractional || fitsInLong) {
      double d = bd.doubleValue();
      var is_finite = d != Double.NEGATIVE_INFINITY && d != Double.POSITIVE_INFINITY;
      if (is_finite) {
        return Double.hashCode(d);
      } else {
        // Infinite values here just means finite values outside the double
        // range. In this path, the values must fractional, and so cannot
        // coincide with a value of any other type (including BigInteger), so we
        // can hash it however we want.
        return bd.hashCode();
      }
    } else {
      // Will not throw ArithmeticException since the value has a 0 fractional part.
      return bd.toBigIntegerExact().hashCode();
    }
  }
}
