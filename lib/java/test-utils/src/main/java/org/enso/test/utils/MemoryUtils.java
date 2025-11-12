package org.enso.test.utils;

import static org.junit.Assert.fail;

import java.lang.ref.Reference;
import java.util.ArrayList;
import org.graalvm.polyglot.Context;

/** Package private now. Use ContextUtils to turn this check on. */
final class MemoryUtils {
  private MemoryUtils() {}

  static void assertGC(String msg, boolean expectGC, Reference<?> ref) {
    var memory = expectGC ? new ArrayList<>() : null;
    var retry = 3;
    for (var i = 1L; ; i *= 2) {
      try {
        var size = (int) Math.min(i, Integer.MAX_VALUE / 2);
        if (i >= 64) {
          tryHarderToGc();
        }
        if (checkAndAlloc(ref, memory, size)) {
          break;
        }
      } catch (OutOfMemoryError err) {
        // launch the JVM with
        //   -XX:+HeapDumpOnOutOfMemoryError
        //   -XX:HeapDumpPath=/tmp
        // to get some info
        if (retry-- <= 0) {
          break;
        }
      }
    }
    assertReference(ref, expectGC, msg, memory);
  }

  private static void assertReference(
      Reference<?> ref, boolean expectGC, String msg, ArrayList<Object> memory) {
    var obj = ref.get();
    if (expectGC) {
      if (obj != null) {
        fail(msg + " ref still alive: " + obj);
      }
    } else {
      if (obj == null) {
        fail(msg + " ref has been cleaned: " + obj);
      }
    }
    System.getLogger("assertGC").log(System.Logger.Level.TRACE, "Cannot GC {0}", memory);
  }

  /**
   * Checks whether {@code ref} has been GCed. If not, it allocates additional memory of given size
   * to create a "presure" on GC to try harder to GC.
   *
   * @param ref
   * @param memory
   * @param toAllocate
   * @return
   */
  private static boolean checkAndAlloc(Reference<?> ref, ArrayList<Object> memory, int toAllocate) {
    if (ref.get() == null) {
      return true;
    }
    System.gc();
    if (memory != null) {
      memory.add(new byte[toAllocate]);
    }
    return false;
  }

  private static void tryHarderToGc() {
    try (var ctx = Context.create()) {
      System.getLogger("assertGC").log(System.Logger.Level.TRACE, "Creating and closing {0}", ctx);
    }
  }
}
