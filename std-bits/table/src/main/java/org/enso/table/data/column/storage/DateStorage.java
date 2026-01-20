package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.nio.IntBuffer;
import java.time.LocalDate;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.util.ImmutableBitSet;

/** A column containing local dates */
public final class DateStorage extends Storage<LocalDate> {

  private final IntBuffer data;
  private final ImmutableBitSet validityMap;

  /** original proxy storage to keep from being garbage collected */
  private final ColumnStorage<?> proxy;

  /**
   * @param data the underlying data
   * @param validityMap a bit set denoting at index {@code i} whether there is a real value at that
   *     index.
   * @param otherStorage reference to proxy storage to prevent it from being GCed while this storage
   *     is used
   */
  public DateStorage(IntBuffer data, ImmutableBitSet validityMap, ColumnStorage<?> otherStorage) {
    super(DateType.INSTANCE);
    this.data = data;
    this.validityMap = validityMap;
    this.proxy = otherStorage;
  }

  @Override
  public long getSize() {
    return data.limit();
  }

  @Override
  public LocalDate getItemBoxed(long index) {
    var at = Math.toIntExact(index);
    if (validityMap.get(at)) {
      var local = data.get(at);
      return LocalDate.ofEpochDay(local);
    } else {
      return null;
    }
  }

  @Override
  public long addressOfData() {
    return MemorySegment.ofBuffer(data).address();
  }

  @Override
  public long addressOfValidity() {
    return MemorySegment.ofBuffer(validityMap.rawData()).address();
  }
}
