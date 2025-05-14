package org.enso.os.environment.jni;

import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.math.BigInteger;

final class TestMain {
  private TestMain() {}

  static void main(String... args) throws Throwable {
    var jvmIsolate = Long.parseLong(args[0]);
    var fnCallbackAddress = MemorySegment.ofAddress(Long.parseLong(args[1]));
    var fnDescriptor =
        FunctionDescriptor.of(
            ValueLayout.JAVA_BOOLEAN,
            ValueLayout.JAVA_LONG,
            ValueLayout.JAVA_LONG,
            ValueLayout.ADDRESS);
    var fnHandle = Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
    var n = Integer.parseInt(args[2]);

    var res = factorial(n).toString();
    reportResultToSvmIsolate(jvmIsolate, fnHandle, n, res);
  }

  static BigInteger factorial(long n) {
    var acc = BigInteger.valueOf(1);
    for (; ; ) {
      acc = acc.multiply(BigInteger.valueOf(n));
      if (--n == 0) {
        break;
      }
    }
    return acc;
  }

  private static void reportResultToSvmIsolate(
      long jvmIsolate, MethodHandle fn, long key, String value) throws Throwable {
    try (var arena = Arena.ofConfined()) {
      var valueStr = arena.allocateFrom(value);
      Object result = fn.invoke(jvmIsolate, key, valueStr);
      if (!Boolean.TRUE.equals(result)) {
        var ex =
            new IllegalStateException(
                "Not correct result for " + key + " and value " + value + " result " + result);
        ex.printStackTrace();
        throw ex;
      }
    }
  }
}
