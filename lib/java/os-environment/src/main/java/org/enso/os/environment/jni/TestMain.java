package org.enso.os.environment.jni;

import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import org.enso.persist.Persistable;

final class TestMain {
  static final Map<Long, String> CORRECT_RESULTS = new HashMap<>();

  private TestMain() {}

  static void main(String... args) throws Throwable {
    var jvmIsolate = Long.parseLong(args[0]);
    var fnCallbackAddress = MemorySegment.ofAddress(Long.parseLong(args[1]));
    var fnDescriptor =
        FunctionDescriptor.of(
            ValueLayout.JAVA_LONG, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG);
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
    ReportResult rr = new ReportResult(key, value);
    JVM.executeImpl(
        JVMPeer.POOL,
        rr,
        (seg) -> {
          Object res = -1L;
          try {
            var isoRef = MemorySegment.ofAddress(jvmIsolate);
            res = fn.invoke(isoRef, seg, seg.byteSize());
          } catch (Throwable ex) {
            ex.printStackTrace();
          }
          return (long) res;
        });
  }

  @Persistable(id = 430606)
  public static final class ReportResult extends JVM.Message<Void> {
    private long key;
    private String value;

    public ReportResult(long key, String value) {
      super(Void.class);
      this.key = key;
      this.value = value;
    }

    public long key() {
      return key;
    }

    public String value() {
      return value;
    }

    @Override
    protected Void evaluate() throws Throwable {
      var vm = System.getProperty("java.vm.name");
      assert "Substrate VM".equals(vm) : "Running in SVM again: " + vm;
      CORRECT_RESULTS.put(key, value);
      return null;
    }
  }
}
