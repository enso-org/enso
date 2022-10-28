package org.enso.table.data.column.storage;

/** A storage containing items representable as a {@code double}. */
public abstract class NumericStorage<T> extends Storage<T> {
  /**
   * Returns the value stored at the given index. The return value if the given index is missing
   * ({@link #isNa(long)}) is undefined.
   *
   * @param idx the index to look up
   * @return the value associated with {@code idx}
   */
  public abstract double getItemDouble(int idx);
}
