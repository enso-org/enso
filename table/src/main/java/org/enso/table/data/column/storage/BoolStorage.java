package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.function.Function;

public class BoolStorage extends Storage {
  private final BitSet values;
  private final BitSet isMissing;
  private final int size;
  private final boolean negated;

  public BoolStorage(BitSet values, BitSet isMissing, int size, boolean negated) {
    this.values = values;
    this.isMissing = isMissing;
    this.size = size;
    this.negated = negated;
  }

  @Override
  public long size() {
    return size;
  }

  @Override
  public long getType() {
    return Type.BOOL;
  }

  public boolean getItem(long idx) {
    return negated != values.get((int) idx);
  }

  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }

  @Override
  public boolean isOpVectorized(VectorizedOp op) {
    return false;
  }

  public BitSet getValues() {
    return values;
  }

  public BitSet getIsMissing() {
    return isMissing;
  }

  @Override
  public Storage mask(BitSet mask, int cardinality) {
    BitSet newMissing = new BitSet();
    BitSet newValues = new BitSet();
    int resultIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        if (isMissing.get(i)) {
          newMissing.set(resultIx++);
        } else if (values.get(i)) {
          newValues.set(resultIx++);
        }
      }
    }
    return new BoolStorage(newValues, newMissing, cardinality, negated);
  }

  public BoolStorage not() {
    return new BoolStorage(values, isMissing, size, !negated);
  }

  @Override
  public Storage map(Function<Object, Object> function) {
    return null;
  }

  public boolean isNegated() {
    return negated;
  }
}
