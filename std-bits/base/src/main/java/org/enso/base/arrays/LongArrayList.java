package org.enso.base.arrays;

import java.util.Arrays;

/** A helper to efficiently build an array of unboxed integers of arbitrary length. */
public class LongArrayList {
  private long[] backingStorage;
  private int lastIndex = -1;

  public LongArrayList() {
    backingStorage = new long[32];
  }

  //** Gets the number of elements in the list. */
  public int getSize() {
    return lastIndex + 1;
  }

  //** Gets an element from the list. */
  public long get(int index) {
    if (index > lastIndex) {
      throw new IndexOutOfBoundsException(index);
    }
    return backingStorage[index];
  }

  //** Gets an element from the list. */
  public long getOrLast(int index) {
    return backingStorage[index > lastIndex ? lastIndex : index];
  }

  //** Adds an element to the list. */
  public synchronized void add(long x) {
    int index, storageIndex;

    index = lastIndex + 1;
    lastIndex = index;
    if (index >= backingStorage.length) {
      backingStorage = Arrays.copyOf(backingStorage, backingStorage.length * 2);
    }

    backingStorage[index] = x;
  }
}
