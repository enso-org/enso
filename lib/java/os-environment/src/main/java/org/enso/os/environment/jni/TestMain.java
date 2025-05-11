package org.enso.os.environment.jni;

import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.math.BigInteger;

final class TestMain {
  private TestMain() {}

  public static void main(String... args) throws Throwable {
    var jvmIsolate = Long.parseLong(args[0]);
    var fnCallbackAddress = MemorySegment.ofAddress(Long.parseLong(args[1]));
    var fnDescriptor =
        FunctionDescriptor.ofVoid(
            ValueLayout.JAVA_LONG, ValueLayout.JAVA_LONG, ValueLayout.JAVA_LONG);
    var fnHandle = Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
    var n = Integer.parseInt(args[2]);

    var res = factorial(n).longValue();
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
      long jvmIsolate, MethodHandle fn, long key, long value) throws Throwable {
    fn.invokeExact(jvmIsolate, key, value);
  }
}
