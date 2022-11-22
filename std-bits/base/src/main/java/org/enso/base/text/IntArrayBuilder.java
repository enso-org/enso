package org.enso.base.text;

/** A helper to efficiently build an array of unboxed integers of arbitrary length. */
public class IntArrayBuilder {
  private int[] storage;
  private int length;

  /**
   * Constructs an empty builder with a given initial capacity.
   *
   * @param initialCapacity the initial capacity of the builder, can be used to avoid expanding the
   *     storage if the amount of elements can be estimated in advance.
   */
  public IntArrayBuilder(int initialCapacity) {
    length = 0;
    storage = new int[initialCapacity];
  }

  /** Adds a new element to the array, expanding it if necessary. */
  public void add(int x) {
    if (length >= storage.length) {
      grow();
    }

    storage[length++] = x;
  }

  /**
   * Expands the storage to fit more elements.
   *
   * <p>The storage grows by 50% and is always increased by at least one. The 50% growth is chosen
   * so that the amortized cost of adding a new element to the array stays constant.
   */
  private void grow() {
    int newCapacity = storage.length + (storage.length / 2);
    if (newCapacity <= storage.length) {
      newCapacity = storage.length + 1;
    }

    int[] newStorage = new int[newCapacity];
    System.arraycopy(this.storage, 0, newStorage, 0, length);
    this.storage = newStorage;
  }

  /** Returns the amount of elements already added to the storage. */
  public int getLength() {
    return length;
  }

  /**
   * Returns the underlying storage of the builder.
   *
   * <p>This method avoids copying for performance so it should be used with care. The storage can
   * actually have more elements than were added, so the user should be careful to only query the
   * first {@code getLength()} elements. Querying other elements results in an unspecified result.
   *
   * <p>After calling this method, the builder is invalidated and cannot be used anymore. Any usage
   * of the builder afterwards will result in a {@code NullPointerException}.
   */
  public int[] unsafeGetStorageAndInvalidateTheBuilder() {
    int[] tmp = storage;
    this.storage = null;
    return tmp;
  }
}
