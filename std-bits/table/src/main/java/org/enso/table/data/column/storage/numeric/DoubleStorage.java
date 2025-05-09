package org.enso.table.data.column.storage.numeric;

import java.math.BigInteger;
import java.util.BitSet;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
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
import org.enso.table.data.column.operation.map.numeric.isin.DoubleIsInOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A column containing floating point numbers. */
public final class DoubleStorage extends Storage<Double>
    implements ColumnDoubleStorage, ColumnStorageWithNothingMap, NumericFormattingStorage {

  final double[] data;
  final BitSet isNothing;
  private final int size;
  private static final MapOperationStorage<Double, DoubleStorage> ops = buildOps();
  private CachedPropertyCheck<Boolean> isNumericFormatRequired;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isNothing a bit set denoting at index {@code i} whether the value at index {@code i} is
   *     Nothing.
   */
  public DoubleStorage(double[] data, int size, BitSet isNothing) {
    this.data = data;
    this.isNothing = isNothing;
    this.size = size;

    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
  }

  public static DoubleStorage makeEmpty(long size) {
    int intSize = Builder.checkSize(size);
    BitSet isNothing = new BitSet(intSize);
    isNothing.set(0, intSize);
    return new DoubleStorage(new double[0], intSize, isNothing);
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public Double getItemBoxed(long idx) {
    return isNothing(idx) ? null : data[Math.toIntExact(idx)];
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    return data[Math.toIntExact(index)];
  }

  @Override
  public FloatType getType() {
    return FloatType.FLOAT_64;
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get((int) idx);
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
    long n = getSize();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      if (isNothing(i)) {
        builder.appendDouble(arg);
      } else {
        builder.appendDouble(getItemAsDouble(i));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /** Special handling to ensure loss of precision is reported. */
  private Storage<?> fillMissingBigInteger(BigInteger arg, ProblemAggregator problemAggregator) {
    long n = getSize();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      if (isNothing(i)) {
        builder.append(arg);
      } else {
        builder.appendDouble(getItemAsDouble(i));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /** Special handling to ensure loss of precision is reported. */
  private Storage<?> fillMissingLong(long arg, ProblemAggregator problemAggregator) {
    long n = getSize();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      if (isNothing(i)) {
        builder.appendLong(arg);
      } else {
        builder.appendDouble(getItemAsDouble(i));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<?> fillMissing(
      Value arg, StorageType<?> commonType, ProblemAggregator problemAggregator) {
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
  public Storage<Double> fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by DoubleStorage.");
    }

    long n = getSize();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, BlackholeProblemAggregator.INSTANCE);
    double previousValue = 0;
    boolean hasPrevious = false;

    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      boolean isCurrentMissing = isNothing(i);
      if (isCurrentMissing) {
        if (hasPrevious) {
          builder.appendDouble(previousValue);
        } else {
          builder.appendNulls(1);
        }
      } else {
        double value = getItemAsDouble(i);
        builder.appendDouble(value);
        previousValue = value;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  public Storage<Double> applyFilter(BitSet filterMask, int newLength) {
    var builder =
        Builder.getForDouble(FloatType.FLOAT_64, newLength, BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (filterMask.get(i)) {
        if (isNothing.get(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendDouble(data[i]);
        }
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<Double> applyMask(OrderMask mask) {
    double[] newData = new double[mask.length()];
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == OrderMask.NOT_FOUND_INDEX || isNothing.get(position)) {
        newIsNothing.set(i);
      } else {
        newData[i] = data[position];
      }

      context.safepoint();
    }
    return new DoubleStorage(newData, newData.length, newIsNothing);
  }

  private static MapOperationStorage<Double, DoubleStorage> buildOps() {
    MapOperationStorage<Double, DoubleStorage> ops = new MapOperationStorage<>();
    ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new DivideOp<>())
        .add(new ModOp<>())
        .add(new PowerOp<>())
        .add(new DoubleRoundOp(Maps.ROUND))
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>())
        .add(new DoubleIsInOp());
    return ops;
  }

  @Override
  public Storage<Double> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    double[] newData;

    // Special case if slice is after the actual data
    if (offset >= data.length) {
      newData = new double[0];
    } else {
      // Can only copy as much as there is data
      int newDataSize = Math.min(data.length - offset, newSize);
      newData = new double[newDataSize];
      System.arraycopy(data, offset, newData, 0, newDataSize);
    }

    BitSet newMask = isNothing.get(offset, offset + limit);
    return new DoubleStorage(newData, newSize, newMask);
  }

  @Override
  public DoubleStorage appendNulls(int count) {
    BitSet newIsNothing = BitSets.makeDuplicate(isNothing);
    newIsNothing.set(size, size + count);

    double[] newData = new double[size + count];
    System.arraycopy(data, 0, newData, 0, size);
    return new DoubleStorage(newData, size + count, newIsNothing);
  }

  @Override
  public Storage<Double> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    double[] newData = new double[newSize];
    BitSet newIsNothing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      for (int i = 0; i < length; ++i) {
        newIsNothing.set(offset + i, isNothing.get(range.start() + i));
        context.safepoint();
      }
      offset += length;
    }

    return new DoubleStorage(newData, newSize, newIsNothing);
  }

  @Override
  public StorageType<?> inferPreciseType(PreciseTypeOptions options) {
    // If we do not request floats becoming integers, then we can return the answer straight away.
    if (!options.wholeFloatsBecomeIntegers()) {
      return getType();
    }

    if (areAllIntegers()) {
      if (options.shrinkIntegers()) {
        return findSmallestIntegerTypeThatFits();
      } else {
        return IntegerType.INT_64;
      }
    }

    return getType();
  }

  private Boolean cachedAreAllIntegers = null;
  private StorageType<?> smallestFittingIntegerType = null;

  private boolean areAllIntegers() {
    if (cachedAreAllIntegers == null) {
      int visitedNumbers = 0;
      boolean areAllIntegers = true;
      for (int i = 0; i < size; i++) {
        if (isNothing.get(i)) {
          continue;
        }

        double value = data[i];
        visitedNumbers++;
        boolean isWholeNumber = value % 1.0 == 0.0;
        boolean canBeInteger = isWholeNumber && IntegerType.INT_64.fits(value);
        if (!canBeInteger) {
          areAllIntegers = false;
          break;
        }
      }

      // We only say 'all are integers' if there was at least one number, because we don't want an
      // empty Float column to change its type for no good reason.
      cachedAreAllIntegers = visitedNumbers > 0 && areAllIntegers;
    }

    return cachedAreAllIntegers;
  }

  private StorageType<?> findSmallestIntegerTypeThatFits() {
    if (smallestFittingIntegerType != null) {
      return smallestFittingIntegerType;
    }

    assert cachedAreAllIntegers;
    final DoubleStorage parent = this;

    // We create a Long storage that gets values by converting our storage.
    ComputedNullableLongStorage longAdapter =
        new ComputedNullableLongStorage(size) {
          @Override
          protected Long computeItem(long idx) {
            if (parent.isNothing(idx)) {
              return null;
            }

            double value = parent.getItemAsDouble(idx);
            assert value % 1.0 == 0.0
                : "The value " + value + " should be a whole number (guaranteed by checks).";
            return (long) value;
          }
        };

    // And rely on its shrinking logic.
    smallestFittingIntegerType = longAdapter.inferPreciseType(PreciseTypeOptions.SHRINK);
    return smallestFittingIntegerType;
  }

  /** Allow access to the underlying data array for copying. */
  public double[] getArray() {
    return data;
  }

  @Override
  public ColumnDoubleStorageIterator iterator() {
    return new DoubleStorageIterator(data, isNothing, (int) getSize());
  }

  private static class DoubleStorageIterator implements ColumnDoubleStorageIterator {
    private final double[] data;
    private final BitSet isNothing;
    private final int size;
    private int index = -1;

    public DoubleStorageIterator(double[] data, BitSet isNothing, int size) {
      this.data = data;
      this.isNothing = isNothing;
      this.size = size;
    }

    @Override
    public Double getItemBoxed() {
      return isNothing.get(index) ? null : data[index];
    }

    @Override
    public double getItemAsDouble() {
      return data[index];
    }

    @Override
    public boolean isNothing() {
      return isNothing.get(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < size;
    }

    @Override
    public Double next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      index++;
      return getItemBoxed();
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }

    @Override
    public void zip(ColumnDoubleStorage otherStorage, DoubleDoubleZipper zipper) {
      Context context = Context.getCurrent();
      var otherSize = otherStorage.getSize();
      var toCount = Math.max(size, otherSize);

      if (otherStorage instanceof DoubleStorage doubleStorage) {
        for (int i = 0; i < toCount; i++) {
          boolean isNothing1 = i >= size || isNothing.get(i);
          double value1 = isNothing1 ? Double.NaN : data[i];
          boolean isNothing2 = i >= otherSize || doubleStorage.isNothing.get(i);
          double value2 = isNothing2 ? Double.NaN : doubleStorage.data[i];
          zipper.accept(i, value1, isNothing1, value2, isNothing2);
          context.safepoint();
        }
      } else {
        int minSize = (int) Math.min(size, otherSize);
        for (int i = 0; i < minSize; i++) {
          boolean isNothing1 = i >= size || isNothing.get(i);
          double value1 = isNothing1 ? 0 : data[i];
          boolean isNothing2 = otherStorage.isNothing(i);
          if (isNothing2) {
            zipper.accept(i, value1, isNothing1, Double.NaN, true);
          } else {
            zipper.accept(i, value1, isNothing1, otherStorage.getItemAsDouble(i), false);
          }
          context.safepoint();
        }

        for (long i = minSize; i < toCount; i++) {
          var isNothing2 = otherStorage.isNothing(i);
          if (isNothing2) {
            zipper.accept(i, 0, true, Double.NaN, true);
          } else {
            zipper.accept(i, 0, true, otherStorage.getItemAsDouble(i), false);
          }
          context.safepoint();
        }
      }
    }

    @Override
    public void zip(ColumnLongStorage otherStorage, DoubleLongZipper zipper) {
      Context context = Context.getCurrent();
      var otherSize = otherStorage.getSize();
      var toCount = Math.max(size, otherSize);

      if (otherStorage instanceof LongStorage longStorage) {
        for (int i = 0; i < toCount; i++) {
          boolean isNothing1 = i >= size || isNothing.get(i);
          double value1 = isNothing1 ? Double.NaN : data[i];
          boolean isNothing2 = i >= otherSize || longStorage.isNothing.get(i);
          long value2 = isNothing2 ? 0 : longStorage.data[i];
          zipper.accept(i, value1, isNothing1, value2, isNothing2);
          context.safepoint();
        }
      } else {
        int minSize = (int) Math.min(size, otherSize);
        for (int i = 0; i < minSize; i++) {
          boolean isNothing1 = i >= size || isNothing.get(i);
          double value1 = isNothing1 ? 0 : data[i];
          var isNothing2 = otherStorage.isNothing(i);
          if (isNothing2) {
            zipper.accept(i, value1, isNothing1, 0, true);
          } else {
            zipper.accept(i, value1, isNothing1, otherStorage.getItemAsLong(i), false);
          }
          context.safepoint();
        }

        for (long i = minSize; i < toCount; i++) {
          var isNothing2 = otherStorage.isNothing(i);
          if (isNothing2) {
            zipper.accept(i, 0, true, 0, true);
          } else {
            zipper.accept(i, 0, true, otherStorage.getItemAsLong(i), false);
          }
          context.safepoint();
        }
      }
    }
  }

  /**
   * Checks if any numbers are large enough for the column to require formatin in the table viz.
   *
   * @return true/false if formatting is required
   */
  @Override
  public Boolean cachedNumericFormatCheck() throws InterruptedException {
    return isNumericFormatRequired.get();
  }
}
