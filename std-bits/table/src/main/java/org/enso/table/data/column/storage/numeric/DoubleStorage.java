package org.enso.table.data.column.storage.numeric;

import java.math.BigInteger;
import java.util.BitSet;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorageIterator;
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
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A column containing floating point numbers. */
public final class DoubleStorage extends Storage<Double>
    implements ColumnDoubleStorage, ColumnStorageWithNothingMap, NumericFormattingStorage {
  final double[] data;
  final BitSet isNothing;
  private final int size;
  private final CachedPropertyCheck<Boolean> isNumericFormatRequired;

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
  public ColumnDoubleStorageIterator iteratorWithIndex() {
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
