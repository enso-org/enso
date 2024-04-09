package org.enso.base.random;

import java.util.Random;
import java.util.function.Function;

/**
 * Container for a `Random` object. Setting the seed requires recreating the `Random` object, so the
 * `Random` should always be obtained through `getCurrentRandom`.
 */
public class RandomInstanceHolder {
  private RandomInstanceHolder() {}

  public RandomInstanceHolder(long seed) {
    setSeed(seed);
  }

  public static Object SINGLETON = null;

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

  public static Object singleton(Function<Object, Object> factory) {
    if (SINGLETON == null) {
      SINGLETON = factory.apply(new RandomInstanceHolder());
    }
    return SINGLETON;
  }
}
