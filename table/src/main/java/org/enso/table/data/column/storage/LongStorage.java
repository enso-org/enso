package org.enso.table.data.column.storage;

import org.enso.table.data.index.Index;

import java.util.BitSet;

/** A column storing 64-bit integers. */
public class LongStorage extends Storage {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isMissing a bit set denoting at index {@code i} whether or not the value at index {@code
   *     i} is missing.
   */
  public LongStorage(long[] data, int size, BitSet isMissing) {
    this.data = data;
    this.isMissing = isMissing;
    this.size = size;
  }

  /** @inheritDoc */
  @Override
  public int size() {
    return size;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public long getItem(long idx) {
    return data[(int) idx];
  }

  @Override
  public Object getItemBoxed(int idx) {
    return isMissing.get(idx) ? null : data[idx];
  }

  /** @inheritDoc */
  @Override
  public long getType() {
    return Type.LONG;
  }

  /** @inheritDoc */
  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }

  @Override
  public boolean isOpVectorized(String op) {
    return Ops.EQ.equals(op) || Ops.IS_MISSING.equals(op);
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    if (Ops.EQ.equals(name)) {
      return runVectorizedEq(operand);
    } else if (Ops.IS_MISSING.equals(name)) {
      return new BoolStorage(isMissing, new BitSet(), size, false);
    }
    throw new UnsupportedOperationException();
  }

  BoolStorage runVectorizedEq(Object operand) {
    BitSet isNa = new BitSet();
    BitSet values = new BitSet();
    if (operand instanceof Long) {
      long seek = (Long) operand;
      for (int i = 0; i < size; i++) {
        if (data[i] == seek && !isMissing.get(i)) {
          values.set(i);
        }
      }
    }
    return new BoolStorage(values, isNa, size, false);
  }

  @Override
  public LongStorage mask(BitSet mask, int cardinality) {
    BitSet newMissing = new BitSet();
    long[] newData = new long[cardinality];
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        if (isMissing.get(i)) {
          newMissing.set(resIx++);
        } else {
          newData[resIx++] = data[i];
        }
      }
    }
    return new LongStorage(newData, cardinality, newMissing);
  }

  @Override
  public Storage orderMask(int[] positions) {
    long[] newData = new long[positions.length];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND || isMissing.get(positions[i])) {
        newMissing.set(i);
      } else {
        newData[i] = data[positions[i]];
      }
    }
    return new LongStorage(newData, positions.length, newMissing);
  }

  @Override
  public Storage countMask(int[] counts, int total) {
    long[] newData = new long[total];
    BitSet newMissing = new BitSet();
    int pos = 0;
    for (int i = 0; i < counts.length; i++) {
      if (isMissing.get(i)) {
        newMissing.set(pos, pos + counts[i]);
        pos += counts[i];
      } else {
        for (int j = 0; j < counts[i]; j++) {
          newData[pos++] = data[i];
        }
      }
    }
    return new LongStorage(newData, total, newMissing);
  }
}
