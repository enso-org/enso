package org.enso.benchmark_helpers;

import java.util.function.Function;

public class PanicsHelpers {
  public static long extendJavaStack(int k, Function<Integer, Long> callback) {
    if (k == 0) {
      return callback.apply(0);
    } else {
      long r = extendJavaStack(k - 1, callback);
      return r + 1;
    }
  }

  public static class MyException extends RuntimeException {}

  public static void throwMyException() {
    throw new MyException();
  }

  public static long iteratePureJava(long n) {
    long acc = 0;
    for (long i = 0; i < n; i++) {
      long d = 0;
      try {
        if (condition(i)) {
          d = 1;
        } else {
          throwMyException();
        }
      } catch (MyException e) {
        d = -1;
      }

      acc += d;
    }

    return acc;
  }

  private static boolean condition(long i) {
    return i % 3 == 0;
  }
}
