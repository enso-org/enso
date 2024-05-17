package org.enso.base.polyglot;

import org.graalvm.polyglot.Value;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The numeric converter deals with conversions of Java numeric types to the two main types
 * supported by Enso - Long for integers and Double for decimals. Any other types are coerced to one
 * of these types.
 *
 * <p>It provides two concepts - coercion - which allows to coerce an integer type to a decimal, but
 * will not convert a decimal to an integer even if it has 0 fractional part. Then there is
 * conversion which allows to convert a decimal with 0 fractional part to an integer. Conversion
 * should be used when we care about the original type of the object (i.e. we want any decimals to
 * require decimal storage even if they have 0 fractional part). Conversion is to be used when we
 * want to be consistent with Enso's equality semantics where 2 == 2.0.
 */
public class NumericConverter {
  public static BigInteger INTEGER_MIN_VALUE_BIG_INTEGER = BigInteger.valueOf(Integer.MIN_VALUE);
  public static BigInteger INTEGER_MAX_VALUE_BIG_INTEGER = BigInteger.valueOf(Integer.MAX_VALUE);
  public static BigDecimal INTEGER_MIN_VALUE_BIG_DECIMAL = BigDecimal.valueOf(Integer.MIN_VALUE);
  public static BigDecimal INTEGER_MAX_VALUE_BIG_DECIMAL = BigDecimal.valueOf(Integer.MAX_VALUE);

  /**
   * Coerces a number (possibly an integer) to a Double.
   *
   * <p>Will throw an exception if the object is not a number.
   */
  public static double coerceToDouble(Object o) {
    return switch (o) {
      case Double x -> x;
      case BigDecimal x -> x.doubleValue();
      case Float x -> x.doubleValue();
      case BigInteger x -> x.doubleValue();
      default -> (double) coerceToLong(o);
    };
  }

  /**
   * Coerces a number to an Integer.
   *
   * <p>Will throw an exception if the object is not an integer.
   *
   * <p>Decimal values are not accepted.
   */
  public static long coerceToLong(Object o) {
    return switch (o) {
      case Long x -> x;
      case Integer x -> x.longValue();
      case Short x -> x.longValue();
      case Byte x -> x.longValue();
      default -> throw new UnsupportedOperationException("Cannot coerce " + o + " to a numeric type.");
    };
  }

  public static BigInteger coerceToBigInteger(Object o) {
    if (o instanceof BigInteger bigInteger) {
      return bigInteger;
    } else {
      long longValue = coerceToLong(o);
      return BigInteger.valueOf(longValue);
    }
  }

  /**
   * Coerces a number to a BigDecimal.
   *
   * <p>Will throw an exception if the object is not a number.
   */
  public static BigDecimal coerceToBigDecimal(Object o) {
    return switch (o) {
      case Double x -> BigDecimal.valueOf(x);
      case BigDecimal x -> x;
      case Float x -> BigDecimal.valueOf(x);
      case BigInteger x -> new BigDecimal(x);
      case Long x -> BigDecimal.valueOf(x);
      case Integer x -> BigDecimal.valueOf(x);
      case Short x -> BigDecimal.valueOf(x);
      case Byte x -> BigDecimal.valueOf(x);
      default -> throw new UnsupportedOperationException("Cannot coerce " + o + " to a BigDecimal.");
    };
  }

  /** Returns true if the object is any supported number. */
  public static boolean isCoercibleToDouble(Object o) {
    return isFloatLike(o)|| isCoercibleToLong(o) || o instanceof BigInteger;
  }

  public static boolean isFloatLike(Object o) {
    return o instanceof Double
        || o instanceof Float;
  }

  /**
   * Returns true if the object is any supported integer.
   *
   * <p>Returns false for decimals with 0 fractional part - the type itself must be an integer type.
   */
  public static boolean isCoercibleToLong(Object o) {
    return o instanceof Long || o instanceof Integer || o instanceof Short || o instanceof Byte;
  }

  public static boolean isCoercibleToBigInteger(Object o) {
    return o instanceof BigInteger || isCoercibleToLong(o);
  }

  /**
   * Tries converting the value to a Double.
   *
   * <p>It will return null if the object represented a non-numeric value.
   */
  public static Double tryConvertingToDouble(Object o) {
    return switch (o) {
      case Double x -> x;
      case BigDecimal x -> x.doubleValue();
      case BigInteger x -> x.doubleValue();
      case Float x -> x.doubleValue();
      case Long x -> x.doubleValue();
      case Integer x -> x.doubleValue();
      case Short x -> x.doubleValue();
      case Byte x -> x.doubleValue();
      case null, default -> null;
    };
  }

  /**
   * Tries converting the value to an Integer.
   *
   * <p>Decimal number types are accepted, only if their fractional part is 0. It will return null
   * if the object represented a non-integer value. Integer values must fit within the Integer range.
   */
  public static Integer tryConvertingToInteger(Object o) {
    return switch (o) {
      case Long x -> fitsInInt(x) ? x.intValue() : null;
      case Integer x -> x;
      case Short x -> (int) x;
      case Byte x -> (int) x;
      case Double x -> integralDoubleToInteger(x);
      case Float x -> integralDoubleToInteger(x);
      case BigDecimal x -> integralBigDecimalToInteger(x);
      case BigInteger x -> integralBigIntegerToInteger(x);
      case null, default -> null;
    };
  }

  private static boolean fitsInInt(long x) {
    return x >= Integer.MIN_VALUE && x <= Integer.MAX_VALUE;
  }

  private static boolean fitsInInt(BigInteger x) {
    return x.compareTo(INTEGER_MIN_VALUE_BIG_INTEGER) >= 0 && x.compareTo(INTEGER_MAX_VALUE_BIG_INTEGER) <= 0;
  }

  private static boolean fitsInInt(BigDecimal x) {
    return x.compareTo(INTEGER_MIN_VALUE_BIG_DECIMAL) >= 0 && x.compareTo(INTEGER_MAX_VALUE_BIG_DECIMAL) <= 0;
  }

  /**
   * Converts a double to an Integer, if the value is integral and fits in the
   * Integer range. Otherwise, returns null;
   */
  private static Integer integralDoubleToInteger(double x) {
    if (x % 1.0 == 0.0) {
      long l = (long) x;
      if (fitsInInt(l)) {
        return (int) l;
      }
    }
    return null;
  }

  /**
   * Converts a BigInteger to an Integer, if the value fits in the Integer
   * range. Otherwise, returns null;
   */
  private static Integer integralBigIntegerToInteger(BigInteger x) {
    if (fitsInInt(x)) {
      return x.intValueExact();
    } else {
      return null;
    }
  }

  /**
   * Converts a BigDecmal to an Integer, if the value is integral and fits in
   * the Integer range. Otherwise, returns null;
   */
  private static Integer integralBigDecimalToInteger(BigDecimal x) {
    if (x.remainder(BigDecimal.ZERO).equals(BigDecimal.ZERO)) {
      if (fitsInInt(x)) {
        return x.intValueExact();
      }
    }
    return null;
  }

  /**
   * Tries converting the value to a Long.
   *
   * <p>Decimal number types are accepted, only if their fractional part is 0. It will return null
   * if the object represented a non-integer value.
   */
  public static Long tryConvertingToLong(Object o) {
    return switch (o) {
      case Long x -> x;
      case Integer x -> x.longValue();
      case Short x -> x.longValue();
      case Byte x -> x.longValue();
      case Double x -> x % 1.0 == 0.0 ? x.longValue() : null;
      case Float x -> x % 1.0f == 0.0f ? x.longValue() : null;
      case BigDecimal x -> {
        try {
          yield x.longValueExact();
        } catch (ArithmeticException e) {
          yield null;
        }
      }
      case BigInteger x -> {
        try {
          yield x.longValueExact();
        } catch (ArithmeticException e) {
          yield null;
        }
      }
      case null, default -> null;
    };
  }

  public static boolean isBigInteger(Value v) {
    return v.fitsInBigInteger() && !v.fitsInLong();
  }

  /** A workaround for <a href="https://github.com/enso-org/enso/issues/7790">#7790</a> */
  public static BigDecimal bigIntegerAsBigDecimal(BigInteger x) {
    return new BigDecimal(x);
  }
}
