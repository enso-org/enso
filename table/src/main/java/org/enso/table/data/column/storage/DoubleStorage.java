package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.numeric.DoubleBooleanOp;
import org.enso.table.data.column.operation.map.numeric.DoubleNumericOp;
import org.enso.table.data.index.Index;

import java.util.BitSet;
import java.util.function.Function;

/** A column containing floating point numbers. */
public class DoubleStorage extends Storage {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;
  private static final MapOpStorage<DoubleStorage> ops = buildOps();

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
  public int size() {
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
    return ops.isSupported(op);
  }

  @Override
  public Storage runVectorizedOp(String name, Object operand) {
    return ops.run(name, this, operand);
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
    return new DoubleStorage(newData, positions.length, newMissing);
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
    return new DoubleStorage(newData, total, newMissing);
  }

  public BitSet getIsMissing() {
    return isMissing;
  }

  private static MapOpStorage<DoubleStorage> buildOps() {
    MapOpStorage<DoubleStorage> ops = new MapOpStorage<>();
    ops.add(
            new DoubleNumericOp(Ops.ADD) {
              @Override
              protected double doDouble(double a, double b) {
                return a + b;
              }
            })
        .add(
            new DoubleNumericOp(Ops.SUB) {
              @Override
              protected double doDouble(double a, double b) {
                return a - b;
              }
            })
        .add(
            new DoubleNumericOp(Ops.MUL) {
              @Override
              protected double doDouble(double a, double b) {
                return a * b;
              }
            })
        .add(
            new DoubleNumericOp(Ops.DIV) {
              @Override
              protected double doDouble(double a, double b) {
                return a / b;
              }
            })
        .add(
            new DoubleBooleanOp(Ops.LT) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a < b;
              }
            })
        .add(
            new DoubleBooleanOp(Ops.LTE) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a <= b;
              }
            })
        .add(
            new DoubleBooleanOp(Ops.EQ) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a == b;
              }

              @Override
              protected boolean doObject(double a, Object o) {
                return false;
              }
            })
        .add(
            new DoubleBooleanOp(Ops.GT) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a > b;
              }
            })
        .add(
            new DoubleBooleanOp(Ops.GTE) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a >= b;
              }
            })
        .add(
            new MapOperation<>(Ops.IS_MISSING) {
              @Override
              public Storage runMap(DoubleStorage storage, Object arg) {
                return new BoolStorage(storage.isMissing, new BitSet(), storage.size, false);
              }

              @Override
              public Storage runZip(DoubleStorage storage, Storage arg) {
                return runMap(storage, null);
              }
            });
    return ops;
  }
}
