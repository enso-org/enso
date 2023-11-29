package org.enso.example;

import java.math.BigInteger;
import java.util.function.Function;

/** A class used for testing Java Interop from Enso code */
public class TestClass {
  public static final int FINAL_ONE = 1;
  public static int nonFinalTwo = 2;
  public static final short SHORT_ONE = 3;
  public static final long LONG_ONE = 4;

  private final Function<Long, Long> function;

  public TestClass(Function<Long, Long> function) {
    this.function = function;
  }

  public TestClass() {
    this(x -> x);
  }

  public static long add(long a, long b) {
    return a + b;
  }

  public static double doubleArrayAverage(double[] arr) {
    var sum = 0.0;
    for (int i = 0; i < arr.length; i++) {
      sum += arr[i];
    }
    return sum / arr.length;
  }

  public static double numberArrayAverage(Number[] arr) {
    var sum = 0.0;
    for (int i = 0; i < arr.length; i++) {
      sum += arr[i].doubleValue();
    }
    return sum / arr.length;
  }

  public static String exactArrayAverage(BigInteger[] arr) {
    var sum = BigInteger.ZERO;
    for (int i = 0; i < arr.length; i++) {
      sum = sum.add(arr[i]);
    }
    return sum.divide(BigInteger.valueOf(arr.length)).toString();
  }

  public long callFunctionAndIncrement(long argument) {
    return function.apply(argument) + 1;
  }

  public void method1() {}

  public void method2() {}

  public static long raiseException(int type) {
    return switch (type) {
      case 0 -> throw new NullPointerException();
      case 1 -> throw new NullPointerException("NPE!");
      default -> 2;
    };
  }

  public static String enumToString(InnerEnum e) {
    return e.toString();
  }

  public static class StaticInnerClass {
    private final String data;

    public StaticInnerClass(String data) {
      this.data = data;
    }

    public String getData() {
      return data;
    }

    public long add(long a, long b) {
      return a + b;
    }

    public static class StaticInnerInnerClass {
      public long mul(long a, long b) {
        return a * b;
      }
    }
  }

  public enum InnerEnum {
    ENUM_VALUE_1,
    ENUM_VALUE_2
  }
}
