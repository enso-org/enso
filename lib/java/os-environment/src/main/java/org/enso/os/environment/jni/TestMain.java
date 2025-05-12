package org.enso.os.environment.jni;

import java.io.File;
import java.io.FileWriter;
import java.math.BigInteger;

public final class TestMain {
  private TestMain() {}

  public static void main(String... args) throws Exception {
    var out = new File(args[0]);
    var n = Integer.parseInt(args[1]);
    try (java.io.FileWriter os = new FileWriter(out)) {
      os.write(factorial(n).toString());
    }
  }

  static BigInteger factorial(int n) {
    var acc = BigInteger.valueOf(1);
    for (; ; ) {
      acc = acc.multiply(BigInteger.valueOf(n));
      if (--n == 0) {
        break;
      }
    }
    return acc;
  }
}
