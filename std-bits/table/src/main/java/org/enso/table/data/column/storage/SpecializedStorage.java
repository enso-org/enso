package org.enso.table.data.column.storage;

import java.util.AbstractList;
import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Context;

public abstract class SpecializedStorage<T> extends Storage<T> {

  protected abstract SpecializedStorage<T> newInstance(T[] data, int size);

  protected abstract T[] newUnderlyingArray(int size);

  @Override
  public abstract StorageType getType();

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  protected SpecializedStorage(
      T[] data, int size, MapOperationStorage<T, SpecializedStorage<T>> ops) {
    this.data = data;
    this.size = size;
    this.ops = ops;
  }

  protected final T[] data;
  protected final int size;
  private final MapOperationStorage<T, SpecializedStorage<T>> ops;

  /** @inheritDoc */
  @Override
  public int size() {
    return size;
  }

  /** @inheritDoc */
  @Override
  public int countMissing() {
    Context context = Context.getCurrent();
    int count = 0;
    for (int i = 0; i < size; i++) {
      if (data[i] == null) {
        count += 1;
      }

      context.safepoint();
    }
    return count;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public T getItem(long idx) {
    return data[(int) idx];
  }

  @Override
  public T getItemBoxed(int idx) {
    return data[idx];
  }

  /** @inheritDoc */
  @Override
  public boolean isNa(long idx) {
    return data[(int) idx] == null;
  }

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return ops.isSupportedUnary(name);
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(String name, MapOperationProblemBuilder problemBuilder) {
    return ops.runUnaryMap(name, this, problemBuilder);
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return ops.isSupportedBinary(name);
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runBinaryMap(name, this, argument, problemBuilder);
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runZip(name, this, argument, problemBuilder);
  }

  @Override
  public SpecializedStorage<T> mask(BitSet mask, int cardinality) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(cardinality);
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newData[resIx++] = data[i];
      }

      context.safepoint();
    }
    return newInstance(newData, cardinality);
  }

  @Override
  public SpecializedStorage<T> applyMask(OrderMask mask) {
    Context context = Context.getCurrent();
    int[] positions = mask.getPositions();
    T[] newData = newUnderlyingArray(positions.length);
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND) {
        newData[i] = null;
      } else {
        newData[i] = data[positions[i]];
      }

      context.safepoint();
    }
    return newInstance(newData, positions.length);
  }

  @Override
  public SpecializedStorage<T> countMask(int[] counts, int total) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(total);
    int pos = 0;
    for (int i = 0; i < counts.length; i++) {
      for (int j = 0; j < counts[i]; j++) {
        newData[pos++] = data[i];
        context.safepoint();
      }
    }
    return newInstance(newData, total);
  }

  public T[] getData() {
    return data;
  }

  @Override
  public SpecializedStorage<T> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    T[] newData = newUnderlyingArray(newSize);
    System.arraycopy(data, offset, newData, 0, newSize);
    return newInstance(newData, newSize);
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

    return newInstance(newData, newSize);
  }

  @Override
  public List<Object> toList() {
    return new ReadOnlyList<>(this);
  }

  private class ReadOnlyList<S> extends AbstractList<Object> {
    private final SpecializedStorage<S> storage;

    public ReadOnlyList(SpecializedStorage<S> storage) {
      this.storage = storage;
    }

    @Override
    public Object get(int index) {
      return storage.getItemBoxed(index);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }
}
