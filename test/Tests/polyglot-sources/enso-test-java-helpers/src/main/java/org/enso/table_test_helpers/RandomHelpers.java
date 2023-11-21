package org.enso.table_test_helpers;

import java.util.Random;

public class RandomHelpers {
  private final Random rng;

  public RandomHelpers(int seed) {
    this.rng = new Random(seed);
  }

  public String makeRandomString(int length) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < length; i++) {
      sb.append((char) rng.nextInt(128));
    }
    return sb.toString();
  }
}
