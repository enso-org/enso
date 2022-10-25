package org.enso.base.polyglot;

import java.math.BigDecimal;

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
      default -> throw new UnsupportedOperationException();
    };
  }

  /** Returns true if the object is any supported number. */
  public static boolean isCoercibleToDouble(Object o) {
    return o instanceof Double
        || o instanceof BigDecimal
        || o instanceof Float
        || isCoercibleToLong(o);
  }

  /**
   * Returns true if the object is any supported integer.
   *
   * <p>Returns false for decimals with 0 fractional part - the type itself must be an integer type.
   */
  public static boolean isCoercibleToLong(Object o) {
    return o instanceof Long || o instanceof Integer || o instanceof Short || o instanceof Byte;
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
      case Float x -> x.doubleValue();
      case Long x -> x.doubleValue();
      case Integer x -> x.doubleValue();
      case Short x -> x.doubleValue();
      case Byte x -> x.doubleValue();
      case null, default -> null;
    };
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
      case null, default -> null;
    };
  }
}
