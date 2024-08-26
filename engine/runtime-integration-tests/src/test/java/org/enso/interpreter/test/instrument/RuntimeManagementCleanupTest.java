package org.enso.interpreter.test.instrument;

import static org.junit.Assert.fail;

import org.junit.Test;

public final class RuntimeManagementCleanupTest {

  @Test
  public void cleanUp() throws InterruptedException {
    var cnt = 0;
    for (var i = 0; i < 100; i++) {
      cnt = 0;
      for (var threadStack : Thread.getAllStackTraces().entrySet()) {
        if (threadStack.getKey().getName().startsWith("Thread-")) {
          System.err.println(threadStack.getKey().getName());
          for (var frame : threadStack.getValue()) {
            System.err.println("  " + frame);
          }
          cnt++;
        }
      }
      System.err.println("Found " + cnt + " suspicious threads");
      if (cnt == 0) {
        return;
      }
      System.gc();
      Thread.sleep(100);
    }
    fail("There are still " + cnt + " suspicious threads");
  }
}
