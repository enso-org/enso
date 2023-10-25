package org.enso.base.random;

import java.util.Random;

/**
 * Container for a `Random` object. Setting the seed requires recreating the `Random` object, so the
 * `Random` should always be obtained through `getCurrentRandom`.
 */
public class RandomInstanceHolder {
  private RandomInstanceHolder() {}

  public RandomInstanceHolder(long seed) {
    setSeed(seed);
  }

  private Random currentRandom = null;

  public Random getCurrentRandom() {
    if (currentRandom == null) {
      setSeed(Random_Utils.getDefaultSeed());
    }
    return currentRandom;
  }

  public void setSeed(long seed) {
    currentRandom = new Random(seed);
  }

  public static final RandomInstanceHolder SINGLETON = new RandomInstanceHolder();
}
