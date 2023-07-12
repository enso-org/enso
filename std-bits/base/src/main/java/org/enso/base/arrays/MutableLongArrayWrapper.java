package org.enso.base.arrays;

/**
 * A wrapper for a long array, used to expose a random-access mutable array to Enso, needed for some
 * efficient algorithms.
 *
 * <p>It is needed, because arrays in Enso are immutable, so we need to use a wrapper like this for
 * algorithms that require efficient mutability.
 */
public class MutableLongArrayWrapper {
  private final long[] storage;

  public MutableLongArrayWrapper(int size) {
    this.storage = new long[size];
  }

  public void set(int ix, long value) {
    storage[ix] = value;
  }

  public long[] getUnderlyingArray() {
    return storage;
  }
}
