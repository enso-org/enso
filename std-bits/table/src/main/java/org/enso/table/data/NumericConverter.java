package org.enso.table.data;

import java.math.BigDecimal;

public class NumericConverter {
  public static double toDouble(Object o) {
    return switch (o) {
      case Double x -> x;
      case BigDecimal x -> x.doubleValue();
      default -> (double) toLong(o);
    };
  }

  public static long toLong(Object o) {
    return switch (o) {
      case Long x -> x;
      case Integer x -> x.longValue();
      case Short x -> x.longValue();
      case Byte x -> x.longValue();
      default -> throw new UnsupportedOperationException();
    };
  }

  public static boolean isCoercibleToDouble(Object o) {
    return o instanceof Double || o instanceof BigDecimal || isCoercibleToLong(o);
  }

  public static boolean isCoercibleToLong(Object o) {
    return o instanceof Long || o instanceof Integer || o instanceof Short || o instanceof Byte;
  }

  public static Double tryToDouble(Object o) {
    return switch (o) {
      case Double x -> x;
      case BigDecimal x -> x.doubleValue();
      case Long x -> x.doubleValue();
      case Integer x -> x.doubleValue();
      case Short x -> x.doubleValue();
      case Byte x -> x.doubleValue();
      case null, default -> null;
    };
  }

  public static Long tryToLong(Object o) {
    return switch (o) {
      case Long x -> x;
      case Integer x -> x.longValue();
      case Short x -> x.longValue();
      case Byte x -> x.longValue();
      case null, default -> null;
    };
  }
}
