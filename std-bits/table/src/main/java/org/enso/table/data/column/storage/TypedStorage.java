package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.util.ImmutableBitSet;

public class TypedStorage<T> extends AbstractBaseStorage<T> {
  private final T[] data;
  private final ColumnStorage<?> proxy;
  private ByteBuffer offheapBuffer;
  private ImmutableBitSet validitySet;

  /**
   * @param data the underlying data
   */
  public TypedStorage(StorageType<T> type, T[] data) {
    this(type, data, null);
  }

  public TypedStorage(StorageType<T> type, T[] data, ColumnStorage<?> proxy) {
    super(type);
    this.data = data;
    this.proxy = proxy;
  }

  @Override
  public long addressOfData() {
    if (offheapBuffer == null) {
      var validity = new BitSet();

      var storageType = StorageType.ofStorage(this);
      offheapBuffer =
          switch (storageType) {
            case TextType _ -> OffHeapStorages.toArrowTextBuffer(data, validity);
            case DateTimeType _ -> OffHeapStorages.toDateTimeBuffer(data, validity);
            case TimeOfDayType _ -> OffHeapStorages.toArrowTimeOfDayBuffer(data, validity);
            default -> null;
          };

      if (offheapBuffer != null) {
        validitySet = new ImmutableBitSet(validity, data.length);
      }
    }

    return offheapBuffer != null ? MemorySegment.ofBuffer(offheapBuffer).address() : 0L;
  }

  @Override
  public long addressOfValidity() {
    if (addressOfData() != 0L) {
      return MemorySegment.ofBuffer(validitySet.rawData()).address();
    }
    return 0L;
  }

  @Override
  public final long getSize() {
    return data.length;
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

  public T[] getData() {
    return data;
  }

  @Override
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }
}
