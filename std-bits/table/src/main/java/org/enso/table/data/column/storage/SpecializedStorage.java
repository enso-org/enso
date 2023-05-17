package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;

import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

public abstract class SpecializedStorage<T> extends Storage<T> {

  protected abstract SpecializedStorage<T> newInstance(T[] data, int size);

  protected abstract T[] newUnderlyingArray(int size);

  @Override
  public abstract StorageType getType();

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  protected SpecializedStorage(T[] data, int size, MapOpStorage<T, SpecializedStorage<T>> ops) {
    this.data = data;
    this.size = size;
    this.ops = ops;
  }

  protected final T[] data;
  protected final int size;
  private final MapOpStorage<T, SpecializedStorage<T>> ops;

  /** @inheritDoc */
  @Override
  public int size() {
    return size;
  }

  /** @inheritDoc */
  @Override
  public int countMissing() {
    int count = 0;
    for (int i = 0; i < size; i++) {
      if (data[i] == null) {
        count += 1;
      }
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
  public boolean isOpVectorized(String name) {
    return ops.isSupported(name);
  }

  @Override
  protected Storage<?> runVectorizedMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runMap(name, this, argument, problemBuilder);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runZip(name, this, argument, problemBuilder);
  }

  @Override
  public SpecializedStorage<T> mask(BitSet mask, int cardinality) {
    T[] newData = newUnderlyingArray(cardinality);
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newData[resIx++] = data[i];
      }
    }
    return newInstance(newData, cardinality);
  }

  @Override
  public SpecializedStorage<T> applyMask(OrderMask mask) {
    int[] positions = mask.getPositions();
    T[] newData = newUnderlyingArray(positions.length);
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND) {
        newData[i] = null;
      } else {
        newData[i] = data[positions[i]];
      }
    }
    return newInstance(newData, positions.length);
  }

  @Override
  public SpecializedStorage<T> countMask(int[] counts, int total) {
    T[] newData = newUnderlyingArray(total);
    int pos = 0;
    for (int i = 0; i < counts.length; i++) {
      for (int j = 0; j < counts[i]; j++) {
        newData[pos++] = data[i];
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
    int newSize = SliceRange.totalLength(ranges);
    T[] newData = newUnderlyingArray(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      offset += length;
    }

    return newInstance(newData, newSize);
  }

  @Override
  public Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    if (targetType == getType()) {
      return this;
    } else if (targetType instanceof AnyObjectType) {
      return new MixedStorageFacade(this);
    } else if (targetType instanceof TextType textType) {
      int n = size();
      StringBuilder builder = new StringBuilder(n);
      for (int i = 0; i < n; i++) {
        Object item = data[i];
        if (item == null) {
          builder.appendNulls(1);
        } else {
          builder.append(item.toString());
        }
      }
      return StringStorage.adapt(builder.seal(), textType);
    } else {
      throw new IllegalStateException("Conversion of " + this.getClass().getSimpleName() + " to " + targetType + " is not supported");
    }
  }
}
