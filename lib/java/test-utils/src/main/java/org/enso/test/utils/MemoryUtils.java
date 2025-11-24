package org.enso.test.utils;

import static org.junit.Assert.fail;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.ref.Reference;
import java.util.ArrayList;
import org.graalvm.polyglot.Context;

/** Package private now. Use ContextUtils to turn this check on. */
final class MemoryUtils {
  private MemoryUtils() {}

  static void assertGC(String msg, boolean expectGC, Reference<?> ref) {
    var buffer = new StringWriter();
    var log = new PrintWriter(buffer);
    var memory = expectGC ? new ArrayList<>() : null;
    var retry = 3;
    for (var i = 1L; ; i *= 2) {
      try {
        var size = (int) Math.min(i, Integer.MAX_VALUE / 2);
        if (i >= 64) {
          tryHarderToGc(log);
          flushLog(buffer);
        }
        if (checkAndAlloc(log, ref, memory, size)) {
          break;
        }
      } catch (OutOfMemoryError err) {
        err.printStackTrace(log);
        flushLog(buffer);
        // launch the JVM with
        //   -XX:+HeapDumpOnOutOfMemoryError
        //   -XX:HeapDumpPath=/tmp
        // to get some info
        if (retry-- <= 0) {
          break;
        }
      }
    }
    assertReference(buffer, ref, expectGC, msg, memory);
  }

  private static void assertReference(
      Object log, Reference<?> ref, boolean expectGC, String msg, ArrayList<Object> memory) {
    var obj = ref.get();
    if (expectGC) {
      if (obj != null) {
        fail(msg + " ref still alive: " + obj + "\n" + log);
      }
    } else {
      if (obj == null) {
        fail(msg + " ref has been cleaned: " + obj + "\n" + log);
      }
    }
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
  private static boolean checkAndAlloc(
      PrintWriter log, Reference<?> ref, ArrayList<Object> memory, int toAllocate) {
    if (ref.get() == null) {
      return true;
    }
    log.println("System.gc()");
    System.gc();
    if (memory != null) {
      log.println("Allocating " + toAllocate + " bytes");
      memory.add(new byte[toAllocate]);
    }
    return false;
  }

  private static void tryHarderToGc(PrintWriter log) {
    for (var t : Thread.getAllStackTraces().entrySet()) {
      var threadName = t.getKey().getName();
      if (threadName.contains("TruffleCompilerThread")) {
        var foundCompilation = false;
        for (var frame : t.getValue()) {
          if ("com.oracle.truffle.runtime.CompilationTask".equals(frame.getClassName())) {
            foundCompilation = true;
          }
        }
        if (foundCompilation) {
          log.println("Found running compiler thread: " + threadName);
          for (var frame : t.getValue()) {
            log.println("  at " + frame);
          }
          try {
            log.println("Sleeping");
            Thread.sleep(1000);
            log.println("Slept 1s, trying again.");
            return;
          } catch (InterruptedException ex) {
            ex.printStackTrace(log);
          }
        }
      }
    }
    try (var ctx = Context.create()) {
      log.println("Creating and closing " + ctx);
    }
  }

  private static void flushLog(StringWriter buffer) {
    System.err.println(buffer);
    buffer.getBuffer().setLength(0);
  }
}
