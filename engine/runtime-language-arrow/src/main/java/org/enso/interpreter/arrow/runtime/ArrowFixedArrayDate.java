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
import java.nio.ByteBuffer;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayDate implements TruffleObject {
  private final int size;
  private final ByteBuffer buffer;

  private final DateUnit unit;

  public ArrowFixedArrayDate(int size, DateUnit unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = allocateBuffer(size * unit.sizeInBytes());
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
    static Object doDay(ArrowFixedArrayDate receiver, long index) {
      // TODO: Needs null bitmap
      var daysSinceEpoch = receiver.buffer.getInt((int) index * receiver.unit.sizeInBytes());
      var localDate = localDateFromDays(daysSinceEpoch);
      return new ArrowDate(localDate);
    }

    @Specialization(guards = "receiver.getUnit() == Millisecond")
    static Object doMilliseconds(ArrowFixedArrayDate receiver, long index) {
      // TODO: Needs null bitmap
      var secondsPlusNanoSinceEpoch =
          receiver.buffer.getLong((int) index * receiver.unit.sizeInBytes());
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
      // TODO: Needs null bitmap
      if (!iop.isDate(value)) {
        throw UnsupportedMessageException.create();
      }
      var at = index * receiver.unit.sizeInBytes();
      var time = iop.asDate(value).toEpochDay();
      receiver.buffer.putInt((int) at, (int) time);
    }

    @Specialization(guards = "receiver.getUnit() == Millisecond")
    static void doMilliseconds(
        ArrowFixedArrayDate receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.isDate(value) || !iop.isTime(value)) {
        throw UnsupportedMessageException.create();
      }

      var at = index * receiver.unit.sizeInBytes();
      if (iop.isTimeZone(value)) {
        var zoneDateTimeInstant =
            instantForZone(iop.asDate(value), iop.asTime(value), iop.asTimeZone(value), utc);
        var secondsPlusNano =
            zoneDateTimeInstant.getEpochSecond() * nanoDiv + zoneDateTimeInstant.getNano();
        receiver.buffer.putLong((int) at, secondsPlusNano);
      } else {
        var dateTime = instantForOffset(iop.asDate(value), iop.asTime(value), ZoneOffset.UTC);
        var secondsPlusNano = dateTime.getEpochSecond() * nanoDiv + dateTime.getNano();
        receiver.buffer.putLong((int) at, secondsPlusNano);
      }
      // TODO: Update nulls bitmap
    }
  }

  @ExportMessage
  final long getArraySize() {
    return size;
  }

  @ExportMessage
  final boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  final boolean isArrayElementModifiable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  final boolean isArrayElementInsertable(long index) {
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
  private static ByteBuffer allocateBuffer(int size) {
    return ByteBuffer.allocate(size);
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

  public enum DateUnit {
    Day(4),
    Millisecond(8);

    private final int bytes;

    DateUnit(int bytes) {
      this.bytes = bytes;
    }

    int sizeInBytes() {
      return bytes;
    }
  }

  private static final long nanoDiv = 1000000000L;

  private static final ZoneId utc = ZoneId.of("UTC");
}
