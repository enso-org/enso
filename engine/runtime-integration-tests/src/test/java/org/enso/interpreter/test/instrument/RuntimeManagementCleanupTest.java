package org.enso.interpreter.test.instrument;

import org.junit.Test;

public final class RuntimeManagementCleanupTest {

  @Test
  public void cleanUp() throws InterruptedException {
    for (var i = 0; i < 100; i++) {
      var cnt = 0;
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
        break;
      }
      System.gc();
      Thread.sleep(100);
    }
  }
}
