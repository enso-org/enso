package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.operation.CountNothing;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Context;

public abstract class SpecializedStorage<T> extends Storage<T> {

  protected abstract SpecializedStorage<T> newInstance(T[] data);

  protected abstract T[] newUnderlyingArray(int size);

  /**
   * @param data the underlying data
   * @param ops the operations supported by this storage
   */
  protected SpecializedStorage(
      StorageType type, T[] data, MapOperationStorage<T, SpecializedStorage<T>> ops) {
    this.type = type;
    this.data = data;
    this.ops = ops;
  }

  protected final T[] data;
  private final StorageType type;
  private final MapOperationStorage<T, SpecializedStorage<T>> ops;

  @Override
  public final long getSize() {
    return data.length;
  }

  @Override
  public StorageType getType() {
    return type;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public T getItemBoxed(long idx) {
    if (idx < 0 || idx >= data.length) {
      throw new IndexOutOfBoundsException(idx);
    }
    return data[(int) idx];
  }

  @Override
  public boolean isNothing(long idx) {
    return this.getItemBoxed(idx) == null;
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return ops.isSupportedBinary(name);
  }

  @Override
  public boolean isTernaryOpVectorized(String op) {
    return ops.isSupportedTernary(op);
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runBinaryMap(name, this, argument, problemAggregator);
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
  public SpecializedStorage<T> applyFilter(BitSet filterMask, int newLength) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(newLength);
    int resIx = 0;
    for (int i = 0; i < data.length; i++) {
      if (filterMask.get(i)) {
        newData[resIx++] = data[i];
      }

      context.safepoint();
    }
    return newInstance(newData);
  }

  @Override
  public SpecializedStorage<T> applyMask(OrderMask mask) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(mask.length());
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      newData[i] = position == OrderMask.NOT_FOUND_INDEX ? null : data[position];
      context.safepoint();
    }
    return newInstance(newData);
  }

  public T[] getData() {
    return data;
  }

  @Override
  public SpecializedStorage<T> slice(int offset, int limit) {
    int newSize = Math.min(data.length - offset, limit);
    T[] newData = newUnderlyingArray(newSize);
    System.arraycopy(data, offset, newData, 0, newSize);
    return newInstance(newData);
  }

  @Override
  public SpecializedStorage<T> slice(List<SliceRange> ranges) {
    Context context = Context.getCurrent();
    int newSize = SliceRange.totalLength(ranges);
    T[] newData = newUnderlyingArray(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      offset += length;
      context.safepoint();
    }

    return newInstance(newData);
  }

  @Override
  public Storage<?> appendNulls(int count) {
    T[] newData = newUnderlyingArray(data.length + count);
    System.arraycopy(data, 0, newData, 0, data.length);
    return newInstance(newData);
  }

  @Override
  public Storage<T> fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null && CountNothing.anyNothing(missingIndicator)) {
      throw new IllegalArgumentException(
          "Missing indicator must not contain missing values itself.");
    }

    T[] newData = newUnderlyingArray(data.length);
    T previous = null;
    boolean hasPrevious = false;

    Context context = Context.getCurrent();
    for (int i = 0; i < data.length; i++) {
      boolean isCurrentValueMissing =
          missingIndicator == null ? isNothing(i) : missingIndicator.getItemAsBoolean(i);
      if (!isCurrentValueMissing) {
        previous = data[i];
        hasPrevious = true;
      }

      newData[i] = hasPrevious ? previous : data[i];
      context.safepoint();
    }

    return newInstance(newData);
  }

  /**
   * Returns the specialized storage casted to my own type, if it is of the same type; or null
   * otherwise.
   */
  @SuppressWarnings("unchecked")
  public SpecializedStorage<T> castIfSameType(SpecializedStorage<?> storage) {
    if (storage.getType().equals(getType())) {
      return (SpecializedStorage<T>) storage;
    } else {
      return null;
    }
  }
}
