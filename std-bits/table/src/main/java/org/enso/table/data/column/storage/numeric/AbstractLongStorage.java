package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.numeric.LongRoundOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.DivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MaxOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MinOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.operation.map.numeric.comparisons.EqualsComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.isin.LongIsInOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.graalvm.polyglot.Context;

public abstract class AbstractLongStorage extends Storage<Long> implements ColumnLongStorage {
  private static final MapOperationStorage<Long, AbstractLongStorage> ops = buildOps();

  private final long size;
  private final IntegerType type;

  protected AbstractLongStorage(long size, IntegerType type) {
    this.size = size;
    this.type = type;
  }

  @Override
  public final long getSize() {
    return size;
  }

  @Override
  public IntegerType getType() {
    return type;
  }

  @Override
  public Long getItemBoxed(long index) {
    return isNothing(index) ? null : getItemAsLong(index);
  }

  @Override
  public abstract boolean isNothing(long idx);

  @Override
  public abstract long getItemAsLong(long index) throws ValueIsNothingException;

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return ops.isSupportedBinary(name);
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

  @Override
  public StorageType inferPreciseType() {
    return getType();
  }

  @Override
  public StorageType inferPreciseTypeShrunk() {
    // If the type is already smallest possible, we return it unchanged (we will return 8-bit
    // columns as-is, although
    // we will not shrink 16-bit columns to 8-bits even if it were possible).
    if (getType().bits().toInteger() <= 16) {
      return getType();
    }

    IntegerType[] possibleTypes =
        new IntegerType[] {IntegerType.INT_16, IntegerType.INT_32, IntegerType.INT_64};

    int currentTypeIdx = 0;
    long n = getSize();
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      if (isNothing(i)) {
        continue;
      }

      long item = getItemAsLong(i);
      while (!possibleTypes[currentTypeIdx].fits(item)) {
        currentTypeIdx++;
      }

      if (currentTypeIdx >= possibleTypes.length - 1) {
        break;
      }

      context.safepoint();
    }

    return possibleTypes[currentTypeIdx];
  }

  private static MapOperationStorage<Long, AbstractLongStorage> buildOps() {
    MapOperationStorage<Long, AbstractLongStorage> ops = new MapOperationStorage<>();
    ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new DivideOp<>())
        .add(new ModOp<>())
        .add(new PowerOp<>())
        .add(new LongRoundOp(Maps.ROUND))
        .add(new MinOp<>())
        .add(new MaxOp<>())
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>())
        .add(new LongIsInOp());
    return ops;
  }

  @Override
  public Storage<Long> fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by AbstractLongStorage.");
    }

    long n = getSize();
    var builder = Builder.getForLong(getType(), n, BlackholeProblemAggregator.INSTANCE);
    long previousValue = 0;
    boolean hasPrevious = false;

    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      boolean isCurrentNothing = isNothing(i);
      if (isCurrentNothing) {
        if (hasPrevious) {
          builder.appendLong(previousValue);
        } else {
          builder.appendNulls(1);
        }
      } else {
        long currentValue = getItemAsLong(i);
        builder.appendLong(currentValue);
        previousValue = currentValue;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return builder.seal();
  }

  /**
   * Return an instance of storage containing the same data but with a wider type.
   *
   * <p>Ideally it should avoid copying the data, if it's possible.
   */
  public abstract AbstractLongStorage widen(IntegerType widerType);

  @Override
  public Storage<Long> applyFilter(BitSet filterMask, int newLength) {
    var builder = Builder.getForLong(getType(), newLength, BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < getSize(); i++) {
      if (filterMask.get(i)) {
        if (isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendLong(getItemAsLong(i));
        }
      }

      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<Long> applyMask(OrderMask mask) {
    var builder = Builder.getForLong(getType(), mask.length(), BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == OrderMask.NOT_FOUND_INDEX || isNothing(position)) {
        builder.appendNulls(1);
      } else {
        builder.appendLong(getItemAsLong(position));
      }

      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<Long> slice(int offset, int limit) {
    int size = (int) getSize();
    int newSize = Math.min(size - offset, limit);
    var builder = Builder.getForLong(getType(), newSize, BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < newSize; i++) {
      if (isNothing(offset + i)) {
        builder.appendNulls(1);
      } else {
        builder.appendLong(getItemAsLong(offset + i));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<Long> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    var builder = Builder.getForLong(getType(), newSize, BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int rangeStart = range.start();
      int length = range.end() - rangeStart;
      for (int i = 0; i < length; i++) {
        if (isNothing(rangeStart + i)) {
          builder.appendNulls(1);
        } else {
          builder.appendLong(getItemAsLong(rangeStart + i));
        }
        context.safepoint();
      }
    }
    return builder.seal();
  }

  @Override
  public Storage<Long> appendNulls(int count) {
    final AbstractLongStorage parent = this;
    int size = (int) parent.getSize();
    return new ComputedNullableLongStorage(size + count) {
      @Override
      protected Long computeItem(int idx) {
        if (idx < size) {
          return parent.getItemBoxed(idx);
        } else {
          return null;
        }
      }
    };
  }
}
