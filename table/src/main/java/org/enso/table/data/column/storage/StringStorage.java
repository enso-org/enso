package org.enso.table.data.column.storage;

import java.util.BitSet;

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

  @Override
  public boolean isOpVectorized(VectorizedOp op) {
    return op == VectorizedOp.EQ;
  }

  public BoolStorage eq(Object that, boolean propagateNa) {
    BitSet values = new BitSet();
    BitSet missing = new BitSet();
    if (!propagateNa) {
      for (int i = 0; i < size; i++) {
        if (!(data[i] == null) && data[i].equals(that)) {
          values.set(i);
        }
      }
    } else {
      for (int i = 0; i < size; i++) {
        if (data[i] == null) {
          missing.set(i);
        } else if (data[i].equals(that)) {
          values.set(i);
        }
      }
    }
    return new BoolStorage(values, missing, size, false);
  }

  @Override
  public StringStorage mask(BitSet mask, int cardinality) {
    String[] newData = new String[cardinality];
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
          newData[resIx++] = data[i];
      }
    }
    return new StringStorage(newData, cardinality);
  }

}
