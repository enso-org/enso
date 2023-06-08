package org.enso.base.arrays;

/**
 * A wrapper for a long array, used to expose a random-access mutable array to Enso, needed for some
 * efficient algorithms.
 */
public class LongArray {
  private final long[] storage;

  public LongArray(int size) {
    this.storage = new long[size];
  }

  public void set(int ix, long value) {
    storage[ix] = value;
  }

  public long[] to_array() {
    return storage;
  }
}
