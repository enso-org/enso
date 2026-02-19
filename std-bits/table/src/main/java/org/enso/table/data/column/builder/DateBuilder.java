package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.time.LocalDate;
import java.util.Objects;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.DateStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for LocalDate columns. */
final class DateBuilder extends ValidityBuilder
    implements BuilderForType<LocalDate>, BuilderWithRetyping {
  private final boolean allowDateToDateTimeConversion;
  private IntBuffer data;

  DateBuilder(int size, boolean allowDateToDateTimeConversion) {
    this(size, 0, 0, allowDateToDateTimeConversion);
  }

  private DateBuilder(int size, long data, long validity, boolean allowDateToDateTimeConversion) {
    super(size, validity);
    this.data = allocBuffer(size, data);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
  }

  private static IntBuffer allocBuffer(int initialSize, long data) {
    var wholeDataSize = Long.BYTES * initialSize;
    ByteBuffer buf;
    if (data == 0L) {
      buf = ByteBuffer.allocateDirect(wholeDataSize).order(ByteOrder.LITTLE_ENDIAN);
    } else {
      var seg = MemorySegment.ofAddress(data).reinterpret(wholeDataSize);
      buf = seg.asByteBuffer().order(ByteOrder.LITTLE_ENDIAN);
    }
    assert buf.capacity() == wholeDataSize;
    assert buf.order() == ByteOrder.LITTLE_ENDIAN;
    return buf.asIntBuffer();
  }

  static DateBuilder fromAddress(int size, long data, long validity) {
    return new DateBuilder(size, data, validity, false);
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalDate;
  }

  @Override
  public DateBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        var local = (LocalDate) o;
        this.setValid(currentSize);
        data.put(currentSize++, Math.toIntExact(local.toEpochDay()));
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(DateType.INSTANCE, o);
      }
    }
    return this;
  }

  @Override
  public DateBuilder appendNulls(int count) {
    doAppendNulls(count);
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    var size = storage.getSize();
    for (var i = 0L; i < size; i++) {
      var item = storage.getItemBoxed(i);
      append(item);
    }
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE);
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    if (allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE)) {
      var res = new DateTimeBuilder(data.capacity(), true);
      for (int i = 0; i < currentSize; i++) {
        res.append(getData(i));
      }
      return res;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  protected int getDataSize() {
    return this.data.capacity();
  }

  @Override
  protected void resize(int desiredCapacity) {
    var newData = allocBuffer(desiredCapacity, 0);
    int toCopy = Math.min(currentSize, data.capacity());
    newData.put(0, this.data, 0, toCopy);
    this.data = newData;
  }

  @Override
  public ColumnStorage<LocalDate> seal() {
    return seal(null);
  }

  final ColumnStorage<LocalDate> seal(ColumnStorage<?> otherStorage) {
    ensureFreeSpaceFor(0);
    var buf = data.asReadOnlyBuffer().position(0).limit(currentSize);
    var validity = this.validityMap();

    return new DateStorage(buf, validity, otherStorage);
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (var i = 0; i < items.length && i < currentSize; i++) {
      items[i] = getData(i);
    }
  }

  private final LocalDate getData(int i) {
    return LocalDate.ofEpochDay(data.get(i));
  }
}
