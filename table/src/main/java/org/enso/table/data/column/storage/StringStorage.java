package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.BoolBuilder;
import org.enso.table.data.column.builder.object.Builder;

import java.util.BitSet;
import java.util.function.Function;

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
  public boolean isOpVectorized(VectorizedOp op) {
    return op == VectorizedOp.EQ;
  }

  public BoolStorage eq(Object that, boolean propagateNa) {
    Object[] data = getData();
    int size = (int) size();
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
    ObjectStorage storage = super.mask(mask, cardinality);
    return new StringStorage(storage.getData(), cardinality);
  }
}
