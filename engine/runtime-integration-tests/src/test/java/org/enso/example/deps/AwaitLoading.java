package org.enso.example.deps;

import static org.junit.Assert.fail;

import java.util.concurrent.CountDownLatch;

public final class AwaitLoading {
  private static final CountDownLatch TWO = new CountDownLatch(2);

  private AwaitLoading() {}

  public static void waitForTwo() {
    TWO.countDown();
    try {
      TWO.await();
    } catch (InterruptedException ex) {
      fail("The test failed with " + ex);
    }
  }
}
