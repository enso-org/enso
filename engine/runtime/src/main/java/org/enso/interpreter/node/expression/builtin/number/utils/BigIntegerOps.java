package org.enso.interpreter.node.expression.builtin.number.utils;

import com.oracle.truffle.api.CompilerDirectives;

import java.math.BigDecimal;
import java.math.BigInteger;

/** Re-exposes big-integer operations behind a truffle boundary. */
public class BigIntegerOps {
  private static final BigInteger MIN_LONG_BIGINT = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger MAX_LONG_BIGINT = BigInteger.valueOf(Long.MAX_VALUE);

  @CompilerDirectives.TruffleBoundary
  public static BigInteger multiply(long a, long b) {
    return BigInteger.valueOf(a).multiply(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger multiply(BigInteger a, long b) {
    return a.multiply(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger multiply(BigInteger a, BigInteger b) {
    return a.multiply(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger add(long a, long b) {
    return BigInteger.valueOf(a).add(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger add(BigInteger a, long b) {
    return a.add(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger add(BigInteger a, BigInteger b) {
    return a.add(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger subtract(long a, long b) {
    return BigInteger.valueOf(a).subtract(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger subtract(BigInteger a, long b) {
    return a.subtract(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger subtract(long a, BigInteger b) {
    return BigInteger.valueOf(a).subtract(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger subtract(BigInteger a, BigInteger b) {
    return a.subtract(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger divide(BigInteger a, long b) {
    return a.divide(BigInteger.valueOf(b));
  }
  @CompilerDirectives.TruffleBoundary
  public static BigInteger divide(BigInteger a, BigInteger b) {
    return a.divide(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger modulo(BigInteger a, long b) {
    return a.mod(BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger modulo(BigInteger a, BigInteger b) {
    return a.mod(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger negate(BigInteger a) {
    return a.negate();
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger negate(long a) {
    return BigInteger.valueOf(a).negate();
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger abs(BigInteger a) {
    return a.abs();
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger abs(long a) {
    return BigInteger.valueOf(a).abs();
  }

  @CompilerDirectives.TruffleBoundary
  public static boolean equals(BigInteger a, BigInteger b) {
    return a.equals(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static double toDouble(BigInteger a) {
    return a.doubleValue();
  }

  @CompilerDirectives.TruffleBoundary
  public static int compare(BigInteger a, BigInteger b) {
    return a.compareTo(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger pow(BigInteger a, long b) {
    BigInteger res = BigInteger.valueOf(1);
    while (b > 0) {
      if (b % 2 == 0) {
        a = a.pow(2);
        b /= 2;
      } else {
        res = res.multiply(a);
        b--;
      }
    }
    return res;
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitAnd(long a, BigInteger b) {
    return BigIntegerOps.bitAnd(BigInteger.valueOf(a), b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitAnd(BigInteger a, long b) {
    return BigIntegerOps.bitAnd(a, BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitAnd(BigInteger a, BigInteger b) {
    return a.and(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitOr(long a, BigInteger b) {
    return BigIntegerOps.bitOr(BigInteger.valueOf(a), b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitOr(BigInteger a, long b) {
    return BigIntegerOps.bitOr(a, BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitOr(BigInteger a, BigInteger b) {
    return a.or(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitXor(long a, BigInteger b) {
    return BigIntegerOps.bitXor(BigInteger.valueOf(a), b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitXor(BigInteger a, long b) {
    return BigIntegerOps.bitXor(a, BigInteger.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitXor(BigInteger a, BigInteger b) {
    return a.xor(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitShiftLeft(long a, int b) {
    return BigIntegerOps.bitShiftLeft(BigInteger.valueOf(a), b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitShiftLeft(BigInteger a, int b) {
    return a.shiftLeft(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitShiftRight(long a, int b) {
    return BigIntegerOps.bitShiftRight(BigInteger.valueOf(a),b);
  }

  @CompilerDirectives.TruffleBoundary
  public static BigInteger bitShiftRight(BigInteger a, int b) {
    return a.shiftRight(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static boolean nonNegative(BigInteger a) {
    return BigIntegerOps.compare(a,BigInteger.ZERO) == 1;
  }

  @CompilerDirectives.TruffleBoundary
  public static boolean isZero(BigInteger a) {
    return BigIntegerOps.compare(a,BigInteger.ZERO) == 0;
  }

  @CompilerDirectives.TruffleBoundary
  public static int compareTo(long a, BigInteger b) {
    return -b.signum();
  }

  @CompilerDirectives.TruffleBoundary
  public static int compareTo(BigInteger a, long b) {
    return a.signum();
  }

  @CompilerDirectives.TruffleBoundary
  public static int compareTo(BigInteger a, BigInteger b) {
    return a.compareTo(b);
  }

  @CompilerDirectives.TruffleBoundary
  public static int compareTo(BigInteger a, double b) {
    return (new BigDecimal(a)).compareTo(BigDecimal.valueOf(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static int compareTo(double a, BigInteger b) {
    return BigDecimal.valueOf(a).compareTo(new BigDecimal(b));
  }

  @CompilerDirectives.TruffleBoundary
  public static boolean fitsInLong(BigInteger bigInteger) {
    return bigInteger.compareTo(MIN_LONG_BIGINT) >= 0 && bigInteger.compareTo(MAX_LONG_BIGINT) <= 0;
  }

  public static boolean fitsInLong(double decimal) {
    return decimal <= Long.MAX_VALUE && decimal >= Long.MIN_VALUE;
  }

  public static boolean fitsInInt(long number) {
    return number >= Integer.MIN_VALUE && number <= Integer.MAX_VALUE;
  }
}
