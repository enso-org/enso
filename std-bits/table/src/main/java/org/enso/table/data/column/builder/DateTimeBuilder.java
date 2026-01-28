package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteOrder;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for ZonedDateTime columns. */
final class DateTimeBuilder extends TypedBuilder<ZonedDateTime> {
  private final boolean allowDateToDateTimeConversion;
  private final BitSet wasLocalDate;

  DateTimeBuilder(int size, boolean allowDateToDateTimeConversion) {
    super(DateTimeType.INSTANCE, new ZonedDateTime[size]);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
    this.wasLocalDate = allowDateToDateTimeConversion ? new BitSet(size) : null;
  }

  static DateTimeBuilder fromAddress(int size, long data, long validity) {
    var validityBuffer =
        MemorySegment.ofAddress(validity).reinterpret((size + 7) / 8).asByteBuffer();
    var bits = BitSet.valueOf(validityBuffer);
    var buf =
        MemorySegment.ofAddress(data)
            .reinterpret(Long.BYTES * size)
            .asByteBuffer()
            .order(ByteOrder.LITTLE_ENDIAN);

    var zonesBuf =
        StringBuilder.fromAddress(size, data + buf.limit(), validity, TextType.VARIABLE_LENGTH);
    var zonesStorage = zonesBuf.seal(null, TextType.VARIABLE_LENGTH);

    var b = new DateTimeBuilder(size, false);
    for (var i = 0; i < size; i++) {
      var stamp = buf.getLong();
      if (bits.get(i)) {
        var nanos = stamp % 1_000_000_000;
        var epochSeconds = stamp / 1_000_000_000;
        var instant = Instant.ofEpochSecond(epochSeconds, nanos);
        var zone = ZoneId.of(zonesStorage.getItemBoxed(i));
        b.append(ZonedDateTime.ofInstant(instant, zone));
      } else {
        b.appendNulls(1);
      }
    }
    return b;
  }

  /**
   * TODO DRY {@link org.enso.table.data.column.operation.cast.ToDateTimeStorageConverter}
   * convertDate.
   */
  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }

  @Override
  public DateTimeBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        if (allowDateToDateTimeConversion && o instanceof LocalDate localDate) {
          data[currentSize++] = convertDate(localDate);
          wasLocalDate.set(currentSize - 1);
        } else if (o instanceof LocalDateTime localDateTime) {
          data[currentSize++] = localDateTime.atZone(ZoneId.systemDefault());
        } else {
          data[currentSize++] = (ZonedDateTime) o;
        }
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateType dateType) {
      var typedStorage = dateType.asTypedStorage(storage);
      long n = typedStorage.getSize();
      for (long i = 0; i < n; i++) {
        var date = typedStorage.getItemBoxed(i);
        this.append(date == null ? null : convertDate(date));
      }
    } else {
      super.appendBulkStorage(storage);
    }
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof ZonedDateTime || (allowDateToDateTimeConversion && o instanceof LocalDate);
  }

  @Override
  protected ColumnStorage<ZonedDateTime> doSeal() {
    return seal(null);
  }

  final ColumnStorage<ZonedDateTime> seal(ColumnStorage<?> other) {
    return new TypedStorage<>(DateTimeType.INSTANCE, data, other);
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (allowDateToDateTimeConversion) {
      if (currentSize >= 0) {
        System.arraycopy(data, 0, items, 0, currentSize);

        // Replace ZonedDateTime with LocalDate where necessary.
        int next = this.wasLocalDate.nextSetBit(0);
        while (next != -1) {
          items[next] = data[next].toLocalDate();
          next = this.wasLocalDate.nextSetBit(next + 1);
        }
      }
    } else {
      super.copyDataTo(items);
    }
  }
}
