package org.enso.table_test_helpers;

import java.util.Random;

public class RandomHelpers {
  private final Random rng;

  public RandomHelpers(int seed) {
    this.rng = new Random(seed);
  }

  public String makeRandomString(int length) {
    StringBuilder sb = new StringBuilder();
    int n = 'z' - 'A';
    for (int i = 0; i < length; i++) {
      sb.append((char) (rng.nextInt(n) + 'A'));
    }
    return sb.toString();
  }
}
