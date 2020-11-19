package org.enso.table.data.column.builder.string;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

/** A column builder appending all the values passed to it in an unchanged form. */
public class StringStorageBuilder extends StorageBuilder {

  private Object[] data;
  private int size;

  /**
   * Creates a new builder from given partial data. Useful for other builders when a type transition
   * is required.
   *
   * @param data the initial data storage
   * @param size the number of already filled elements
   */
  public StringStorageBuilder(String[] data, int size) {
    this.data = data;
    this.size = size;
  }

  /** Creates an empty builder. */
  public StringStorageBuilder() {
    data = new String[64];
    size = 0;
  }

  /** @inheritDoc */
  @Override
  public StorageBuilder parseAndAppend(String value) {
    ensureAppendable();
    data[size++] = value;
    return this;
  }

  private void ensureAppendable() {
    if (size >= data.length) {
      Object[] newData = new Object[2 * data.length];
      System.arraycopy(data, 0, newData, 0, data.length);
      data = newData;
    }
  }

  /** @inheritDoc */
  @Override
  public Storage seal() {
    return new StringStorage(data, size);
  }
}
