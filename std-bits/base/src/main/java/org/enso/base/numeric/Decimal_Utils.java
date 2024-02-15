package org.enso.base.numeric;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;

/** Utils for the Enso Decmial type. */
public class Decimal_Utils {
  public static record ConversionResult(BigDecimal newValue, boolean hasPrecisionLoss) {}

  public static BigDecimal fromString(String s) {
    return new BigDecimal(s);
  }

  public static ConversionResult fromString(String s, MathContext mc) {
    BigDecimal bd = new BigDecimal(s, mc);
    BigDecimal withoutMC = new BigDecimal(s);
    return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
  }

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
      BigDecimal withoutMC = new BigDecimal(l);
      long backToLong = bd.longValue();
      return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
    } else if (o instanceof BigInteger bi) {
      BigDecimal bd = new BigDecimal(bi, mc);
      BigDecimal withoutMC = new BigDecimal(bi);
      BigInteger backToBigInteger = bd.toBigInteger();
      return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
    } else {
      throw new IllegalArgumentException("Input must be Long or BigInteger");
    }
  }

  public static BigDecimal fromEnsoFloat(Double d) {
    System.out.println("fEF " + d);
    // According to the BigInteger Javadocs, valueOf is preferred because "the
    // value returned is equal to that resulting from constructing a BigDecimal
    // from the result of using Double.toString(double)."
    return BigDecimal.valueOf(d);
  }

  public static ConversionResult fromEnsoFloat(Double d, MathContext mc) {
    System.out.println("fEF mc " + d);
    BigDecimal bd = new BigDecimal(d, mc);
    BigDecimal withoutMC = new BigDecimal(d);
    double backToDouble = bd.doubleValue();
    return new ConversionResult(bd, bd.compareTo(withoutMC) != 0);
  }
}
