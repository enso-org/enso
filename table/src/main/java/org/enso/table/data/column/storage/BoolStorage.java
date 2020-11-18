package org.enso.table.data.column.storage;

import java.util.BitSet;

/**
 * A boolean column storage.
 */
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

  @Override
  public Object getItemBoxed(int idx) {
    return isMissing.get(idx) ? null : values.get(idx);
  }

  public boolean getItem(long idx) {
    return negated != values.get((int) idx);
  }

  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }

  @Override
  public boolean isOpVectorized(String op) {
    return op.equals(Ops.EQ) || op.equals(Ops.NOT);
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    if (Ops.EQ.equals(name)) {
      return runVectorizedEq(operand);
    } else if (Ops.NOT.equals(name)) {
      return new BoolStorage(values, isMissing, size, !negated);
    }
    throw new UnsupportedOperationException();
  }

  private BoolStorage runVectorizedEq(Object operand) {
    if (operand instanceof Boolean) {
      if ((Boolean) operand) {
        return this;
      } else {
        BitSet newVals = new BitSet();
        newVals.or(values);
        newVals.flip(0, size);
        newVals.andNot(isMissing);
        return new BoolStorage(newVals, new BitSet(), size, false);
      }
    } else {
      return new BoolStorage(new BitSet(), new BitSet(), size, false);
    }
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

  public boolean isNegated() {
    return negated;
  }
}
