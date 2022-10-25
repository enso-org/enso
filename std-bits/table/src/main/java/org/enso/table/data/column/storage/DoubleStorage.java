package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.numeric.DoubleBooleanOp;
import org.enso.table.data.column.operation.map.numeric.DoubleIsInOp;
import org.enso.table.data.column.operation.map.numeric.DoubleNumericOp;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Value;

/** A column containing floating point numbers. */
public final class DoubleStorage extends NumericStorage<Double> {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;
  private static final MapOpStorage<Double, DoubleStorage> ops = buildOps();

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

  /** @inheritDoc */
  @Override
  public int countMissing() {
    return isMissing.cardinality();
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public double getItem(long idx) {
    return Double.longBitsToDouble(data[(int) idx]);
  }

  @Override
  public double getItemDouble(int idx) {
    return getItem(idx);
  }

  @Override
  public Double getItemBoxed(int idx) {
    return isMissing.get(idx) ? null : Double.longBitsToDouble(data[idx]);
  }

  /** @inheritDoc */
  @Override
  public int getType() {
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
  protected Storage<?> runVectorizedMap(String name, Object argument) {
    return ops.runMap(name, this, argument);
  }

  @Override
  protected Storage<?> runVectorizedZip(String name, Storage<?> argument) {
    return ops.runZip(name, this, argument);
  }

  private Storage<?> fillMissingDouble(double arg) {
    final var builder = NumericBuilder.createDoubleBuilder(size());
    long rawArg = Double.doubleToRawLongBits(arg);
    for (int i = 0; i < size(); i++) {
      if (isMissing.get(i)) {
        builder.appendRawNoGrow(rawArg);
      } else {
        builder.appendRawNoGrow(data[i]);
      }
    }
    return builder.seal();
  }

  @Override
  public Storage<?> fillMissing(Value arg) {
    if (arg.isNumber()) {
      if (arg.fitsInLong()) {
        return fillMissingDouble(arg.asLong());
      } else if (arg.fitsInDouble()) {
        return fillMissingDouble(arg.asDouble());
      }
    }

    return super.fillMissing(arg);
  }

  @Override
  public Storage<Double> mask(BitSet mask, int cardinality) {
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
  public Storage<Double> applyMask(OrderMask mask) {
    int[] positions = mask.getPositions();
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
  public Storage<Double> countMask(int[] counts, int total) {
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

  private static MapOpStorage<Double, DoubleStorage> buildOps() {
    MapOpStorage<Double, DoubleStorage> ops = new MapOpStorage<>();
    ops.add(
            new DoubleNumericOp(Maps.ADD) {
              @Override
              protected double doDouble(double a, double b) {
                return a + b;
              }
            })
        .add(
            new DoubleNumericOp(Maps.SUB) {
              @Override
              protected double doDouble(double a, double b) {
                return a - b;
              }
            })
        .add(
            new DoubleNumericOp(Maps.MUL) {
              @Override
              protected double doDouble(double a, double b) {
                return a * b;
              }
            })
        .add(
            new DoubleNumericOp(Maps.DIV) {
              @Override
              protected double doDouble(double a, double b) {
                return a / b;
              }
            })
        .add(
            new DoubleNumericOp(Maps.MOD) {
              @Override
              protected double doDouble(double a, double b) {
                return a % b;
              }
            })
        .add(
            new DoubleBooleanOp(Maps.LT) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a < b;
              }
            })
        .add(
            new DoubleBooleanOp(Maps.LTE) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a <= b;
              }
            })
        .add(
            new DoubleBooleanOp(Maps.EQ) {
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
            new DoubleBooleanOp(Maps.GT) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a > b;
              }
            })
        .add(
            new DoubleBooleanOp(Maps.GTE) {
              @Override
              protected boolean doDouble(double a, double b) {
                return a >= b;
              }
            })
        .add(
            new UnaryMapOperation<>(Maps.IS_MISSING) {
              @Override
              public BoolStorage run(DoubleStorage storage) {
                return new BoolStorage(storage.isMissing, new BitSet(), storage.size, false);
              }
            })
        .add(
            new UnaryMapOperation<>(Maps.IS_NAN) {
              @Override
              public BoolStorage run(DoubleStorage storage) {
                BitSet nans = new BitSet();
                for (int i = 0; i < storage.size; i++) {
                  if (!storage.isNa(i) && Double.isNaN(storage.getItem(i))) {
                    nans.set(i);
                  }
                }
                return new BoolStorage(nans, new BitSet(), storage.size, false);
              }
            })
        .add(new DoubleIsInOp());
    return ops;
  }

  @Override
  public Storage<Double> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    long[] newData = new long[newSize];
    System.arraycopy(data, offset, newData, 0, newSize);
    BitSet newMask = isMissing.get(offset, offset + limit);
    return new DoubleStorage(newData, newSize, newMask);
  }

  @Override
  public Storage<Double> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newMissing = new BitSet(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      for (int i = 0; i < length; ++i) {
        newMissing.set(offset + i, isMissing.get(range.start() + i));
      }
      offset += length;
    }

    return new DoubleStorage(newData, newSize, newMissing);
  }
}
