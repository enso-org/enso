package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayDate implements TruffleObject {
  private final int size;
  private final ByteBufferDirect buffer;

  private final DateUnit unit;

  public ArrowFixedArrayDate(int size, DateUnit unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = allocateBuffer(size * unit.sizeInBytes(), size);
  }

  public ArrowFixedArrayDate(ByteBufferDirect buffer, DateUnit unit)
      throws UnsupportedMessageException {
    this.size = buffer.capacity() / unit.sizeInBytes();
    this.unit = unit;
    this.buffer = buffer;
  }

  public DateUnit getUnit() {
    return unit;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  @ImportStatic(ArrowFixedArrayDate.DateUnit.class)
  static class ReadArrayElement {
    @Specialization(guards = "receiver.getUnit() == Day")
    static Object doDay(ArrowFixedArrayDate receiver, long index)
        throws UnsupportedMessageException {
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      var daysSinceEpoch = receiver.buffer.getInt(at);
      var localDate = localDateFromDays(daysSinceEpoch);
      return new ArrowDate(localDate);
    }

    @Specialization(guards = "receiver.getUnit() == Millisecond")
    static Object doMilliseconds(ArrowFixedArrayDate receiver, long index)
        throws UnsupportedMessageException {
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      var secondsPlusNanoSinceEpoch = receiver.buffer.getLong(at);
      var seconds = Math.floorDiv(secondsPlusNanoSinceEpoch, nanoDiv);
      var nano = Math.floorMod(secondsPlusNanoSinceEpoch, nanoDiv);
      var zonedDateTime = zonedDateTimeFromSeconds(seconds, nano, utc);
      return new ArrowZonedDateTime(zonedDateTime);
    }
  }

  @ExportMessage
  @ImportStatic(ArrowFixedArrayDate.DateUnit.class)
  static class WriteArrayElement {
    @Specialization(guards = "receiver.getUnit() == Day")
    static void doDay(
        ArrowFixedArrayDate receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.isDate(value)) {
        throw UnsupportedMessageException.create();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      var time = iop.asDate(value).toEpochDay();
      receiver.buffer.putInt(at, Math.toIntExact(time));
    }

    @Specialization(guards = {"receiver.getUnit() == Millisecond", "!iop.isNull(value)"})
    static void doMilliseconds(
        ArrowFixedArrayDate receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.isDate(value) || !iop.isTime(value)) {
        throw UnsupportedMessageException.create();
      }

      var at = typeAdjustedIndex(index, receiver.unit);
      if (iop.isTimeZone(value)) {
        var zoneDateTimeInstant =
            instantForZone(iop.asDate(value), iop.asTime(value), iop.asTimeZone(value), utc);
        var secondsPlusNano =
            zoneDateTimeInstant.getEpochSecond() * nanoDiv + zoneDateTimeInstant.getNano();
        receiver.buffer.putLong(at, secondsPlusNano);
      } else {
        var dateTime = instantForOffset(iop.asDate(value), iop.asTime(value), ZoneOffset.UTC);
        var secondsPlusNano = dateTime.getEpochSecond() * nanoDiv + dateTime.getNano();
        receiver.buffer.putLong(at, secondsPlusNano);
      }
    }

    @Specialization(guards = "iop.isNull(value)")
    static void doNull(
        ArrowFixedArrayDate receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop) {
      receiver.buffer.setNull((int) index);
    }
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return index >= 0 && index < size;
  }

  @ExportLibrary(InteropLibrary.class)
  static class ArrowDate implements TruffleObject {
    private LocalDate date;

    public ArrowDate(LocalDate date) {
      this.date = date;
    }

    @ExportMessage
    public boolean isDate() {
      return true;
    }

    @ExportMessage
    public LocalDate asDate() {
      return date;
    }
  }

  @ExportLibrary(InteropLibrary.class)
  static class ArrowZonedDateTime implements TruffleObject {
    private ZonedDateTime dateTime;

    public ArrowZonedDateTime(ZonedDateTime dateTime) {
      this.dateTime = dateTime;
    }

    @ExportMessage
    public boolean isDate() {
      return true;
    }

    @ExportMessage
    public LocalDate asDate() {
      return dateTime.toLocalDate();
    }

    @ExportMessage
    public boolean isTime() {
      return true;
    }

    @ExportMessage
    public LocalTime asTime() {
      return dateTime.toLocalTime();
    }

    @ExportMessage
    public boolean isTimeZone() {
      return true;
    }

    @ExportMessage
    public ZoneId asTimeZone() {
      return dateTime.getZone();
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static ByteBufferDirect allocateBuffer(int sizeInBytes, int size) {
    return new ByteBufferDirect(sizeInBytes, size);
  }

  @CompilerDirectives.TruffleBoundary
  private static LocalDate localDateFromDays(int daysSinceEpoch) {
    return LocalDate.ofEpochDay(daysSinceEpoch);
  }

  @CompilerDirectives.TruffleBoundary
  private static ZonedDateTime zonedDateTimeFromSeconds(long seconds, long nano, ZoneId zone) {
    return Instant.ofEpochSecond(seconds, nano).atZone(zone);
  }

  @CompilerDirectives.TruffleBoundary
  private static Instant instantForZone(
      LocalDate date, LocalTime time, ZoneId zone, ZoneId target) {
    return date.atTime(time).atZone(zone).withZoneSameLocal(target).toInstant();
  }

  @CompilerDirectives.TruffleBoundary
  private static Instant instantForOffset(LocalDate date, LocalTime time, ZoneOffset offset) {
    return date.atTime(time).toInstant(offset);
  }

  public enum DateUnit implements SizeInBytes {
    Day(4),
    Millisecond(8);

    private final int bytes;

    DateUnit(int bytes) {
      this.bytes = bytes;
    }

    public int sizeInBytes() {
      return bytes;
    }
  }

  private static final long nanoDiv = 1000000000L;

  private static final ZoneId utc = ZoneId.of("UTC");

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return Math.toIntExact(index * unit.sizeInBytes());
  }
}
