package org.enso.exploratory_benchmark_helpers;

import java.util.function.Function;

public class PanicsHelpers {
  public static int extendJavaStack(int k, Function<Integer, Object> callback) {
    if (k == 0) {
      callback.apply(0);
      return 0;
    } else {
      int r = extendJavaStack(k - 1, callback);
      return r + 1;
    }
  }

  public static class MyException extends RuntimeException {
  }

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
