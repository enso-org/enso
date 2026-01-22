package org.enso.example.deps;

import static org.junit.Assert.fail;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public final class AwaitLoading {
  private static final CountDownLatch TWO = new CountDownLatch(2);

  private AwaitLoading() {}

  public static void waitForTwo() {
    TWO.countDown();
    try {
      TWO.await(1, TimeUnit.SECONDS);
    } catch (InterruptedException ex) {
      fail("The test failed with " + ex);
    }
  }
}
