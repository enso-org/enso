package org.enso.table.data.column.storage.numeric;

import java.math.BigInteger;
import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.numeric.DoubleLongMapOpWithSpecialNumericHandling;
import org.enso.table.data.column.operation.map.numeric.DoubleRoundOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.DivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.operation.map.numeric.comparisons.EqualsComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.isin.DoubleIsInOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A column containing floating point numbers. */
public final class DoubleStorage extends NumericStorage<Double> implements DoubleArrayAdapter {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;
  private static final MapOperationStorage<Double, DoubleStorage> ops = buildOps();

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isMissing a bit set denoting at index {@code i} whether the value at index {@code i} is
   *     missing.
   */
  public DoubleStorage(long[] data, int size, BitSet isMissing) {
    this.data = data;
    this.isMissing = isMissing;
    this.size = size;
  }

  public static DoubleStorage makeEmpty(int size) {
    BitSet isMissing = new BitSet(size);
    isMissing.set(0, size);
    return new DoubleStorage(new long[0], size, isMissing);
  }

  /**
   * @inheritDoc
   */
  @Override
  public int size() {
    return size;
  }

  /**
   * @inheritDoc
   */
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
  public Double getItemBoxed(int idx) {
    return isMissing.get(idx) ? null : Double.longBitsToDouble(data[idx]);
  }

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return ops.isSupportedUnary(name);
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(
      String name, MapOperationProblemAggregator problemAggregator) {
    return ops.runUnaryMap(name, this, problemAggregator);
  }

  /**
   * @inheritDoc
   */
  @Override
  public StorageType getType() {
    return FloatType.FLOAT_64;
  }

  /**
   * @inheritDoc
   */
  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }

  @Override
  public double getItemAsDouble(int i) {
    return Double.longBitsToDouble(data[i]);
  }

  @Override
  public boolean isNa(int i) {
    return isMissing.get(i);
  }

  @Override
  public boolean isBinaryOpVectorized(String op) {
    return ops.isSupportedBinary(op);
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runBinaryMap(name, this, argument, problemAggregator);
  }

  @Override
  public boolean isTernaryOpVectorized(String op) {
    return ops.isSupportedTernary(op);
  }

  @Override
  public Storage<?> runVectorizedTernaryMap(
      String name,
      Object argument0,
      Object argument1,
      MapOperationProblemAggregator problemAggregator) {
    return ops.runTernaryMap(name, this, argument0, argument1, problemAggregator);
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runZip(name, this, argument, problemAggregator);
  }

  private Storage<?> fillMissingDouble(double arg, ProblemAggregator problemAggregator) {
    final var builder = NumericBuilder.createDoubleBuilder(size(), problemAggregator);
    long rawArg = Double.doubleToRawLongBits(arg);
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      if (isMissing.get(i)) {
        builder.appendRawNoGrow(rawArg);
      } else {
        builder.appendRawNoGrow(data[i]);
      }

      context.safepoint();
    }
    return builder.seal();
  }

  /** Special handling to ensure loss of precision is reported. */
  private Storage<?> fillMissingBigInteger(BigInteger arg, ProblemAggregator problemAggregator) {
    final var builder = NumericBuilder.createDoubleBuilder(size(), problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      if (isMissing.get(i)) {
        builder.appendBigInteger(arg);
      } else {
        builder.appendRawNoGrow(data[i]);
      }

      context.safepoint();
    }
    return builder.seal();
  }

  /** Special handling to ensure loss of precision is reported. */
  private Storage<?> fillMissingLong(long arg, ProblemAggregator problemAggregator) {
    final var builder = NumericBuilder.createDoubleBuilder(size(), problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      if (isMissing.get(i)) {
        builder.appendLong(arg);
      } else {
        builder.appendRawNoGrow(data[i]);
      }

      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<?> fillMissing(
      Value arg, StorageType commonType, ProblemAggregator problemAggregator) {
    if (arg.isNumber()) {
      if (arg.fitsInLong()) {
        return fillMissingLong(arg.asLong(), problemAggregator);
      } else if (arg.fitsInBigInteger()) {
        return fillMissingBigInteger(arg.asBigInteger(), problemAggregator);
      } else if (arg.fitsInDouble()) {
        return fillMissingDouble(arg.asDouble(), problemAggregator);
      }
    }

    return super.fillMissing(arg, commonType, problemAggregator);
  }

  @Override
  public DoubleStorage fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by DoubleStorage.");
    }

    int n = size();
    long[] newData = new long[n];
    BitSet newMissing = new BitSet();
    long previousValueRaw = 0;
    boolean hasPrevious = false;

    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      boolean isCurrentMissing = isNa(i);
      if (isCurrentMissing) {
        if (hasPrevious) {
          newData[i] = previousValueRaw;
        } else {
          newMissing.set(i);
        }
      } else {
        long currentValueRaw = data[i];
        newData[i] = currentValueRaw;
        previousValueRaw = currentValueRaw;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return new DoubleStorage(newData, n, newMissing);
  }

  @Override
  public Storage<Double> mask(BitSet mask, int cardinality) {
    BitSet newMissing = new BitSet();
    long[] newData = new long[cardinality];
    int resIx = 0;
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        if (isMissing.get(i)) {
          newMissing.set(resIx++);
        } else {
          newData[resIx++] = data[i];
        }
      }

      context.safepoint();
    }
    return new DoubleStorage(newData, cardinality, newMissing);
  }

  @Override
  public Storage<Double> applyMask(OrderMask mask) {
    int[] positions = mask.getPositions();
    long[] newData = new long[positions.length];
    BitSet newMissing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND || isMissing.get(positions[i])) {
        newMissing.set(i);
      } else {
        newData[i] = data[positions[i]];
      }

      context.safepoint();
    }
    return new DoubleStorage(newData, positions.length, newMissing);
  }

  @Override
  public Storage<Double> countMask(int[] counts, int total) {
    long[] newData = new long[total];
    BitSet newMissing = new BitSet();
    int pos = 0;
    Context context = Context.getCurrent();
    for (int i = 0; i < counts.length; i++) {
      if (isMissing.get(i)) {
        newMissing.set(pos, pos + counts[i]);
        pos += counts[i];
      } else {
        for (int j = 0; j < counts[i]; j++) {
          newData[pos++] = data[i];
        }
      }

      context.safepoint();
    }
    return new DoubleStorage(newData, total, newMissing);
  }

  public BitSet getIsMissing() {
    return isMissing;
  }

  public long[] getRawData() {
    return data;
  }

  private static MapOperationStorage<Double, DoubleStorage> buildOps() {
    MapOperationStorage<Double, DoubleStorage> ops = new MapOperationStorage<>();
    ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new DivideOp<>())
        .add(new ModOp<>())
        .add(new PowerOp<>())
        .add(
            new DoubleLongMapOpWithSpecialNumericHandling(Maps.TRUNCATE) {
              @Override
              protected long doOperation(double a) {
                return (long) a;
              }
            })
        .add(
            new DoubleLongMapOpWithSpecialNumericHandling(Maps.CEIL) {
              @Override
              protected long doOperation(double a) {
                return (long) Math.ceil(a);
              }
            })
        .add(
            new DoubleLongMapOpWithSpecialNumericHandling(Maps.FLOOR) {
              @Override
              protected long doOperation(double a) {
                return (long) Math.floor(a);
              }
            })
        .add(new DoubleRoundOp(Maps.ROUND))
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>())
        .add(
            new UnaryMapOperation<>(Maps.IS_NOTHING) {
              @Override
              public BoolStorage runUnaryMap(
                  DoubleStorage storage, MapOperationProblemAggregator problemAggregator) {
                return new BoolStorage(storage.isMissing, new BitSet(), storage.size, false);
              }
            })
        .add(
            new UnaryMapOperation<>(Maps.IS_NAN) {
              @Override
              public BoolStorage runUnaryMap(
                  DoubleStorage storage, MapOperationProblemAggregator problemAggregator) {
                BitSet nans = new BitSet();
                Context context = Context.getCurrent();
                for (int i = 0; i < storage.size; i++) {
                  if (!storage.isNa(i) && Double.isNaN(storage.getItemAsDouble(i))) {
                    nans.set(i);
                  }

                  context.safepoint();
                }
                return new BoolStorage(nans, storage.isMissing, storage.size, false);
              }
            })
        .add(
            new UnaryMapOperation<>(Maps.IS_INFINITE) {
              @Override
              public BoolStorage runUnaryMap(
                  DoubleStorage storage, MapOperationProblemAggregator problemAggregator) {
                BitSet infintes = new BitSet();
                Context context = Context.getCurrent();
                for (int i = 0; i < storage.size; i++) {
                  if (!storage.isNa(i) && Double.isInfinite(storage.getItemAsDouble(i))) {
                    infintes.set(i);
                  }

                  context.safepoint();
                }
                return new BoolStorage(infintes, storage.isMissing, storage.size, false);
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
  public DoubleStorage appendNulls(int count) {
    BitSet newMissing = BitSets.makeDuplicate(isMissing);
    newMissing.set(size, size + count);

    long[] newData = new long[size + count];
    System.arraycopy(data, 0, newData, 0, size);
    return new DoubleStorage(newData, size + count, newMissing);
  }

  @Override
  public Storage<Double> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newMissing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      for (int i = 0; i < length; ++i) {
        newMissing.set(offset + i, isMissing.get(range.start() + i));
        context.safepoint();
      }
      offset += length;
    }

    return new DoubleStorage(newData, newSize, newMissing);
  }

  private StorageType inferredType = null;

  @Override
  public StorageType inferPreciseType() {
    if (inferredType == null) {
      boolean areAllIntegers = true;
      int visitedNumbers = 0;
      for (int i = 0; i < size; i++) {
        if (isMissing.get(i)) {
          continue;
        }

        double value = Double.longBitsToDouble(data[i]);
        visitedNumbers++;
        boolean isWholeNumber = value % 1.0 == 0.0;
        boolean canBeInteger = isWholeNumber && IntegerType.INT_64.fits(value);
        if (!canBeInteger) {
          areAllIntegers = false;
          break;
        }
      }

      // We only switch to integers if there was at least one number.
      inferredType = (areAllIntegers && visitedNumbers > 0) ? IntegerType.INT_64 : getType();
    }

    return inferredType;
  }

  @Override
  public StorageType inferPreciseTypeShrunk() {
    StorageType inferred = inferPreciseType();
    if (inferred instanceof IntegerType) {
      return findSmallestIntegerTypeThatFits();
    } else {
      return inferred;
    }
  }

  private StorageType findSmallestIntegerTypeThatFits() {
    assert inferredType instanceof IntegerType;

    final DoubleStorage parent = this;

    // We create a Long storage that gets values by converting our storage.
    ComputedNullableLongStorage longAdapter =
        new ComputedNullableLongStorage(size) {
          @Override
          protected Long computeItem(int idx) {
            if (parent.isNa(idx)) {
              return null;
            }

            double value = parent.getItem(idx);
            assert value % 1.0 == 0.0
                : "The value " + value + " should be a whole number (guaranteed by checks).";
            return (long) value;
          }
        };

    // And rely on its shrinking logic.
    return longAdapter.inferPreciseTypeShrunk();
  }
}
