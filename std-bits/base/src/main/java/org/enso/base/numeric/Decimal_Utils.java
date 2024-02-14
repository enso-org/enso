package org.enso.base.numeric;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;

/** Utils for the Enso Decmial type. */
public class Decimal_Utils {
  public static BigDecimal fromEnsoInteger(Object o) {
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

  public static BigDecimal fromEnsoInteger(Object o, MathContext mc) {
    if (o instanceof Long l) {
      return new BigDecimal(l, mc);
    } else if (o instanceof BigInteger bi) {
      return new BigDecimal(bi, mc);
    } else {
      throw new IllegalArgumentException("Input must be Long or BigInteger");
    }
  }

  public static BigDecimal fromEnsoFloat(Double d) {
    System.out.println("AAA " + d + " " + d.getClass());
    // According to the BigInteger Javadocs, valueOf is preferred because "the
    // value returned is equal to that resulting from constructing a BigDecimal
    // from the result of using Double.toString(double)."
    return BigDecimal.valueOf(d);
  }

  public static BigDecimal fromEnsoFloat(Double d, MathContext mc) {
    System.out.println("AAAmc " + d + " " + d.getClass() + " " + mc);
    return new BigDecimal(d, mc);
  }
}
