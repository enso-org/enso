package org.enso.base.text;

public class IntArrayBuilder {
  private int[] storage;
  private int length;

  public IntArrayBuilder(int initialCapacity) {
    length = 0;
    storage = new int[initialCapacity];
  }

  public void add(int x) {
    if (length >= storage.length) {
      grow();
    }

    storage[length++] = x;
  }

  private void grow() {
    int newCapacity = storage.length + (storage.length / 2);
    if (newCapacity <= storage.length) {
      newCapacity = storage.length + 1;
    }

    int[] newStorage = new int[newCapacity];
    System.arraycopy(this.storage, 0, newStorage, 0, length);
    this.storage = newStorage;
  }

  /**
   * Returns the underlying storage of the builder.
   *
   * <p>This method avoids copying for performance so it should be used with care. The builder
   * should not be used after consuming the result as it could modify it. Moreover, the storage can
   * actually have more elements than were added.
   */
  public int[] unsafeGetStorage() {
    return storage;
  }
}
