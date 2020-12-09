package org.enso.table.data.column.storage;

import org.enso.table.data.index.Index;

import java.util.BitSet;

/** A column storing strings. */
public class StringStorage extends ObjectStorage {

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public StringStorage(Object[] data, int size) {
    super(data, size);
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public String getItem(long idx) {
    return (String) super.getItem(idx);
  }

  /** @inheritDoc */
  @Override
  public long getType() {
    return Type.STRING;
  }

  @Override
  public StringStorage mask(BitSet mask, int cardinality) {
    ObjectStorage storage = super.mask(mask, cardinality);
    return new StringStorage(storage.getData(), cardinality);
  }

  @Override
  public StringStorage orderMask(int[] positions) {
    ObjectStorage storage = super.orderMask(positions);
    return new StringStorage(storage.getData(), (int) storage.size());
  }

  @Override
  public StringStorage countMask(int[] counts, int total) {
    ObjectStorage storage = super.countMask(counts, total);
    return new StringStorage(storage.getData(), total);
  }
}
