package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.bool.BooleanIsInOp;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A boolean column storage. */
public final class BoolStorage extends Storage<Boolean>
    implements ColumnBooleanStorage, ColumnStorageWithNothingMap {
  private static final MapOperationStorage<Boolean, BoolStorage> ops = buildOps();
  private final BitSet values;
  private final BitSet isNothing;
  private final int size;
  private final boolean negated;

  public BoolStorage(BitSet values, BitSet isNothing, int size, boolean negated) {
    this.values = values;
    this.isNothing = isNothing;
    this.size = size;
    this.negated = negated;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public Boolean getItemBoxed(long idx) {
    return isNothing(idx) ? null : getItemAsBoolean(idx);
  }

  @Override
  public StorageType<Boolean> getType() {
    return BooleanType.INSTANCE;
  }

  @Override
  public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }

    return negated != values.get((int) index);
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get((int) idx);
  }

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
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runZip(name, this, argument, problemAggregator);
  }

  public boolean isNegated() {
    return negated;
  }

  public BitSet getValues() {
    return values;
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  /**
   * Creates a new BoolStorage in which all missing values have been replaced by arg.
   *
   * <p>It works by setting the new isMissing to an empty bitset and changing the values bitset
   * accordingly. If `arg` is true, new values are `values || isMissing` and if `arg` is false, new
   * values are `values && (~isMissing)`.
   */
  public BoolStorage fillMissingBoolean(boolean arg) {
    final var newValues = (BitSet) values.clone();
    if (arg != negated) {
      newValues.or(isNothing);
    } else {
      newValues.andNot(isNothing);
    }
    return new BoolStorage(newValues, new BitSet(), size, negated);
  }

  @Override
  public Storage<?> fillMissing(
      Value arg, StorageType<?> commonType, ProblemAggregator problemAggregator) {
    if (arg.isBoolean()) {
      return fillMissingBoolean(arg.asBoolean());
    } else {
      return super.fillMissing(arg, commonType, problemAggregator);
    }
  }

  @Override
  public Storage<?> fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by BoolStorage.");
    }

    boolean previousValue = false;
    boolean hasPrevious = false;
    long size = getSize();
    var builder = Builder.getForBoolean(size);

    Context context = Context.getCurrent();
    for (long i = 0; i < size; i++) {
      boolean isCurrentValueMissing = isNothing(i);
      if (isCurrentValueMissing) {
        if (hasPrevious) {
          builder.appendBoolean(previousValue);
        } else {
          builder.appendNulls(1);
        }
      } else {
        boolean currentValue = getItemAsBoolean(i);
        builder.appendBoolean(currentValue);
        previousValue = currentValue;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  public Storage<Boolean> applyFilter(BitSet filterMask, int newLength) {
    Context context = Context.getCurrent();
    var builder = Builder.getForBoolean(newLength);
    for (int i = 0; i < size; i++) {
      if (filterMask.get(i)) {
        if (isNothing.get(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendBoolean(getItemAsBoolean(i));
        }
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public Storage<Boolean> applyMask(OrderMask mask) {
    Context context = Context.getCurrent();
    var builder = Builder.getForBoolean(mask.length());
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == OrderMask.NOT_FOUND_INDEX || isNothing.get(position)) {
        builder.appendNulls(1);
      } else {
        builder.appendBoolean(getItemAsBoolean(position));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  private static MapOperationStorage<Boolean, BoolStorage> buildOps() {
    MapOperationStorage<Boolean, BoolStorage> ops = new MapOperationStorage<>();
    ops.add(new BooleanIsInOp());
    return ops;
  }

  /** Creates a mask that selects elements corresponding to true entries in the passed storage. */
  public static BitSet toMask(BoolStorage storage) {
    BitSet mask = storage.normalize();
    mask.andNot(storage.getIsNothingMap());
    return mask;
  }

  /**
   * Returns a BitSet representation of the storage. It is the same as the values BitSet, but with
   * an assumption that the negated flag is false.
   */
  private BitSet normalize() {
    BitSet set = new BitSet();
    set.or(this.values);
    if (this.negated) {
      set.flip(0, this.size);
    }
    return set;
  }

  @Override
  public BoolStorage slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    return new BoolStorage(
        values.get(offset, offset + limit),
        isNothing.get(offset, offset + limit),
        newSize,
        negated);
  }

  @Override
  public Storage<?> appendNulls(int count) {
    BitSet isNothing = BitSets.makeDuplicate(this.isNothing);
    isNothing.set(size, size + count);
    return new BoolStorage(values, isNothing, size + count, negated);
  }

  @Override
  public Storage<Boolean> slice(List<SliceRange> ranges) {
    Context context = Context.getCurrent();
    int newSize = SliceRange.totalLength(ranges);
    var builder = Builder.getForBoolean(newSize);
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      for (int i = 0; i < length; ++i) {
        if (isNothing.get(range.start() + i)) {
          builder.appendNulls(1);
        } else {
          builder.appendBoolean(getItemAsBoolean(range.start() + i));
        }
        context.safepoint();
      }
    }
    return builder.seal();
  }

  @Override
  public ColumnBooleanStorageIterator iteratorWithIndex() {
    return new BoolStorageIterator(this);
  }

  private static class BoolStorageIterator implements ColumnBooleanStorageIterator {
    private final BoolStorage parent;
    private int index = -1;

    public BoolStorageIterator(BoolStorage parent) {
      this.parent = parent;
    }

    @Override
    public Boolean getItemBoxed() {
      return parent.getItemBoxed(index);
    }

    @Override
    public boolean getItemAsBoolean() {
      return !parent.isNothing(index) && parent.getItemAsBoolean(index);
    }

    @Override
    public boolean isNothing() {
      return parent.isNothing(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < parent.getSize();
    }

    @Override
    public Boolean next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      return parent.getItemBoxed(++index);
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
}
