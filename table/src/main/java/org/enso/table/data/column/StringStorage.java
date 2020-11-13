package org.enso.table.data.column;

/** A column storing strings. */
public class StringStorage extends Storage {
  private final String[] data;
  private final int size;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public StringStorage(String[] data, int size) {
    this.data = data;
    this.size = size;
  }

  /** @inheritDoc */
  @Override
  public long size() {
    return size;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public String getItem(long idx) {
    return data[(int) idx];
  }

  /** @inheritDoc */
  @Override
  public long getType() {
    return Type.STRING;
  }

  /** @inheritDoc */
  @Override
  public boolean isNa(long idx) {
    return data[(int) idx] == null;
  }
}
