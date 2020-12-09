package org.enso.table.data.column.storage;

import org.enso.table.data.index.Index;

import java.util.BitSet;

/** A boolean column storage. */
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
  public int size() {
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
    return op.equals(Ops.EQ) || op.equals(Ops.NOT) || op.equals(Ops.IS_MISSING);
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    if (Ops.EQ.equals(name)) {
      return runVectorizedEq(operand);
    } else if (Ops.NOT.equals(name)) {
      return new BoolStorage(values, isMissing, size, !negated);
    } else if (Ops.IS_MISSING.equals(name)) {
      return new BoolStorage(isMissing, new BitSet(), size, false);
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

  @Override
  public Storage orderMask(int[] positions) {
    BitSet newNa = new BitSet();
    BitSet newVals = new BitSet();
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND || isMissing.get(positions[i])) {
        newNa.set(i);
      } else if (values.get(positions[i])) {
        values.set(i);
      }
    }
    return new BoolStorage(newVals, newNa, positions.length, negated);
  }

  @Override
  public Storage countMask(int[] counts, int total) {
    BitSet newNa = new BitSet();
    BitSet newVals = new BitSet();
    int pos = 0;
    for (int i = 0; i < counts.length; i++) {
      if (isMissing.get(i)) {
        newNa.set(pos, pos + counts[i]);
      } else if (values.get(i)) {
        newVals.set(pos, pos + counts[i]);
      }
      pos += counts[i];
    }
    return new BoolStorage(newVals, newNa, total, negated);
  }

  public boolean isNegated() {
    return negated;
  }
}
