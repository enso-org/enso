package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.function.Function;

/** A column containing floating point numbers. */
public class DoubleStorage extends Storage {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;
  private static final long NAN = 0x7ff0000000000000L;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isMissing a bit set denoting at index {@code i} whether or not the value at index {@code
   *     i} is missing.
   */
  public DoubleStorage(long[] data, int size, BitSet isMissing) {
    this.data = data;
    this.isMissing = isMissing;
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
  public double getItem(long idx) {
    return Double.longBitsToDouble(data[(int) idx]);
  }

  @Override
  public Object getItemBoxed(int idx) {
    return isMissing.get(idx) ? null : Double.longBitsToDouble(data[idx]);
  }

  /** @inheritDoc */
  @Override
  public long getType() {
    return Type.DOUBLE;
  }

  /** @inheritDoc */
  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }

  @Override
  public boolean isOpVectorized(String op) {
    return op.equals("==");
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    if (name.equals("==")) {
      return runVectorizedEq(operand);
    }
    throw new UnsupportedOperationException();
  }

  private BoolStorage runVectorizedEq(Object operand) {
    BitSet isNa = new BitSet();
    BitSet values = new BitSet();
    if (operand instanceof Double) {
      long seek = Double.doubleToRawLongBits((Double) operand);
      if ((seek & NAN) != NAN) {
        for (int i = 0; i < size; i++) {
          if (data[i] == seek && (data[i] & NAN) != NAN && !isMissing.get(i)) {
            values.set(i);
          }
        }
      }
    }
    return new BoolStorage(values, isNa, size, false);
  }

  @Override
  public DoubleStorage mask(BitSet mask, int cardinality) {
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
    return new DoubleStorage(newData, cardinality, newMissing);
  }
}
