package org.enso.interpreter.test.instrument;

import static org.junit.Assert.fail;

import java.io.OutputStream;
import java.io.PrintStream;
import org.junit.Test;

public final class RuntimeManagementCleanupTest {

  @Test
  public void cleanUp() throws InterruptedException {
    var cnt = 0;
    for (var i = 1; i < 100; i++) {
      cnt = 0;
      var err = i % 10 == 0 ? System.err : new PrintStream(OutputStream.nullOutputStream());
      for (var threadStack : Thread.getAllStackTraces().entrySet()) {
        if (threadStack.getKey().getName().startsWith("Thread-")) {
          err.println(threadStack.getKey().getName());
          for (var frame : threadStack.getValue()) {
            err.println("  " + frame);
          }
          cnt++;
        }
      }
      err.println("Found " + cnt + " suspicious threads");
      if (cnt == 0) {
        return;
      }
      System.gc();
      Thread.sleep(100);
    }
    fail("There are still " + cnt + " suspicious threads");
  }
}
