package org.enso.base.arrays;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/** A helper to efficiently build an array of unboxed integers of arbitrary length. */
public class LongArrayList {
  private final List<long[]> backingStorage;
  private int lastIndex = -1;
  private final Lock lock;

  public LongArrayList() {
    backingStorage = new ArrayList<>();
    backingStorage.add(new long[16]);

    lock = new ReentrantLock();
  }

  private int getStorageIndex(int index) {
    int shifted = index >> 4;
    int count = 0;
    while (shifted > 0) {
      count++;
      shifted >>= 1;
    }
    return count;
  }

  public int getSize() {
    return lastIndex + 1;
  }

  public long get(int index) {
    if (index > lastIndex) {
      throw new IndexOutOfBoundsException();
    }

    int storageIndex = getStorageIndex(index);
    long[] store = backingStorage.get(storageIndex);
    return store[index - (storageIndex == 0 ? 0 : store.length)];
  }

  public void add(long x) {
    int index, storageIndex;

    // This needs to be locked to ensure if used in a multi-threaded environment no issue.
    lock.lock();
    try {
      index = lastIndex + 1;
      lastIndex = index;
      storageIndex = getStorageIndex(index);
      if (storageIndex >= backingStorage.size()) {
        backingStorage.add(
            new long
                [backingStorage.get(backingStorage.size() - 1).length
                    * (storageIndex == 1 ? 1 : 2)]);
      }
    } finally {
      lock.unlock();
    }

    long[] store = backingStorage.get(storageIndex);
    store[index - (storageIndex == 0 ? 0 : store.length)] = x;
  }

  public long[] toArray() {
    long[] result = new long[lastIndex + 1];
    for (int i = 0; i < backingStorage.size(); i++) {
      long[] store = backingStorage.get(i);
      int toCopy =
          i < backingStorage.size() - 1 ? store.length : lastIndex - (i == 0 ? 0 : store.length);
      System.arraycopy(store, 0, result, i == 0 ? 0 : store.length, toCopy);
    }
    return result;
  }
}
