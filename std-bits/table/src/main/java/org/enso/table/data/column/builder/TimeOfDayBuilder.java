package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteOrder;
import java.time.LocalTime;
import java.util.BitSet;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for LocalTime columns. */
final class TimeOfDayBuilder extends TypedBuilder<LocalTime> {
  TimeOfDayBuilder(int size) {
    super(TimeOfDayType.INSTANCE, new LocalTime[size]);
  }

  static TimeOfDayBuilder fromAddress(int size, long data, long validity) {
    var validityBuffer =
        MemorySegment.ofAddress(validity).reinterpret((size + 7) / 8).asByteBuffer();
    var bits = BitSet.valueOf(validityBuffer);
    var buf =
        MemorySegment.ofAddress(data)
            .reinterpret(Long.BYTES * size)
            .asByteBuffer()
            .order(ByteOrder.LITTLE_ENDIAN);

    var b = new TimeOfDayBuilder(size);
    for (var i = 0; i < size; i++) {
      var day = buf.getLong();
      if (bits.get(i)) {
        b.append(LocalTime.ofNanoOfDay(day));
      } else {
        b.appendNulls(1);
      }
    }
    return b;
  }

  @Override
  public TimeOfDayBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        data[currentSize++] = (LocalTime) o;
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(getStorageType(), o);
      }
    }
    return this;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalTime;
  }

  final ColumnStorage<LocalTime> seal(ColumnStorage<?> otherStorage) {
    return new TypedStorage<>(getStorageType(), data, otherStorage);
  }
}
