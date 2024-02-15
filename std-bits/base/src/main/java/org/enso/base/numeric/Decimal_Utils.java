package org.enso.base.numeric;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;

/** Utils for the Enso Decmial type. */
public class Decimal_Utils {
  public static record ConversionResult(BigDecimal newValue, bool hasPrecisionLoss) {}

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

  public static ConversionResult fromEnsoInteger(Object o, MathContext mc) {
    if (o instanceof Long l) {
      BigDecimal bd = new BigDecimal(l, mc);
      long backToLong = bd.longValue();
      System.out.println("AAA l " + o + " " + (l == backToLong) + " " + l + " " + backToLong + " " + (l-backToLong) );
      return new ConversionResult(bd, l != backToLong);
    } else if (o instanceof BigInteger bi) {
      BigDecimal bd = new BigDecimal(bi, mc);
      BigInteger backToBigInteger = bd.toBigInteger();
      System.out.println("AAA bi " + o + " " + (bi.compareTo(backToBigInteger) == 0) + " " + bi + " " + backToBigInteger + " " + (bi.subtract(backToBigInteger)));
      return new ConversionResult(bd, bi.compareTo(backToBigInteger) != 0);
    } else {
      throw new IllegalArgumentException("Input must be Long or BigInteger");
    }
  }

  public static BigDecimal fromEnsoFloat(Double d) {
    // According to the BigInteger Javadocs, valueOf is preferred because "the
    // value returned is equal to that resulting from constructing a BigDecimal
    // from the result of using Double.toString(double)."
    return BigDecimal.valueOf(d);
  }

  public static BigDecimal fromEnsoFloat(Double d, MathContext mc) {
    BigDecimal bd = new BigDecimal(d, mc);
    double backToDouble = bd.doubleValue();
    System.out.println("AAA d " + d + " " + (d == backToDouble) + " " + d + " " + backToDouble + " " + (d-backToDouble));
    return new ConversionResult(bd, d != backToDouble);
  }
}
