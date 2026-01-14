package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteOrder;
import java.time.LocalDate;
import java.util.BitSet;
import java.util.Objects;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for LocalDate columns. */
final class DateBuilder extends TypedBuilder<LocalDate> {
  private final boolean allowDateToDateTimeConversion;

  DateBuilder(int size, boolean allowDateToDateTimeConversion) {
    super(DateType.INSTANCE, new LocalDate[size]);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
  }

  static DateBuilder fromAddress(int size, long data, long validity) {
    var validityBuffer =
        MemorySegment.ofAddress(validity).reinterpret((size + 7) / 8).asByteBuffer();
    var bits = BitSet.valueOf(validityBuffer);
    var buf =
        MemorySegment.ofAddress(data)
            .reinterpret(Integer.BYTES * size)
            .asByteBuffer()
            .order(ByteOrder.LITTLE_ENDIAN);

    var b = new DateBuilder(size, false);
    for (var i = 0; i < size; i++) {
      var day = buf.getInt();
      if (bits.get(i)) {
        b.append(LocalDate.ofEpochDay(day));
      } else {
        b.appendNulls(1);
      }
    }
    return b;
  }

  @Override
  public DateBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        data[currentSize++] = (LocalDate) o;
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
    return this;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalDate;
  }

  @Override
  protected ColumnStorage<LocalDate> doSeal() {
    return seal(null, DateType.INSTANCE);
  }

  final Storage<LocalDate> seal(ColumnStorage<?> otherStorage, DateType type) {
    return new TypedStorage<>(type, data, otherStorage);
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    if (allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE)) {
      return true;
    }
    return super.canRetypeTo(type);
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    if (allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE)) {
      var res = new DateTimeBuilder(data.length, true);
      for (int i = 0; i < currentSize; i++) {
        res.append(data[i]);
      }
      return res;
    }
    return super.retypeTo(type);
  }
}
