package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.BoolBuilder;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;

import java.util.BitSet;
import java.util.function.Function;

/** A column storing arbitrary objects. */
public class ObjectStorage extends Storage {
  private final Object[] data;
  private final int size;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public ObjectStorage(Object[] data, int size) {
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
  public Object getItem(long idx) {
    return data[(int) idx];
  }

  @Override
  public Object getItemBoxed(int idx) {
    return data[idx];
  }

  /** @inheritDoc */
  @Override
  public long getType() {
    return Type.OBJECT;
  }

  /** @inheritDoc */
  @Override
  public boolean isNa(long idx) {
    return data[(int) idx] == null;
  }

  @Override
  public boolean isOpVectorized(String op) {
    return Ops.IS_MISSING.equals(op);
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    if (Ops.IS_MISSING.equals(name)) {
      return runIsMissing();
    }
    throw new UnsupportedOperationException();
  }

  private BoolStorage runIsMissing() {
    BitSet vals = new BitSet();
    for (int i = 0; i < size; i++) {
      if (data[i] == null) {
        vals.set(i);
      }
    }
    return new BoolStorage(vals, new BitSet(), size, false);
  }

  @Override
  public ObjectStorage mask(BitSet mask, int cardinality) {
    Object[] newData = new Object[cardinality];
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newData[resIx++] = data[i];
      }
    }
    return new ObjectStorage(newData, cardinality);
  }

  @Override
  public Storage orderMask(int[] positions) {
    return null;
  }

  public Object[] getData() {
    return data;
  }
}
