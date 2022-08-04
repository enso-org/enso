package org.enso.base;

import java.util.Arrays;
import java.util.Random;

public class Random_Utils {
  /** Samples k random values from the input. */
  public static Object[] sample(Object[] array, int k, Random rng) {
    k = Math.min(k, array.length);
    var copy = Arrays.copyOf(array, array.length);
    shuffleFirstInPlace(copy, k, rng);
    return Arrays.copyOf(copy, k);
  }

  public static Long[] random_indices(int n, int k, Random rng) {
    /*
     * TODO while acceptable for `k` close to `n`, for `k <<< n`, this algorithm is not efficient, a
     * better one should be implemented, see: https://www.pivotaltracker.com/story/show/182853142
     */
    Long[] indices = new Long[n];
    for (int i = 0; i < n; ++i) {
      indices[i] = (long) i;
    }
    k = Math.min(k, n);
    shuffleFirstInPlace(indices, k, rng);
    return Arrays.copyOf(indices, k);
  }

  private static <T> void shuffleFirstInPlace(T[] array, int k, Random rng) {
    int n = array.length;
    for (int i = 0; i < Math.min(k, n); ++i) {
      int r = i + rng.nextInt(n - i);
      T tmp = array[i];
      array[i] = array[r];
      array[r] = tmp;
    }
  }
}
