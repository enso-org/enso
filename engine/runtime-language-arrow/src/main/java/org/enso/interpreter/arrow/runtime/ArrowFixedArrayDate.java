package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayDate implements TruffleObject {
  private final int size;
  private final ByteBufferDirect buffer;

  private final LogicalLayout unit;

  public ArrowFixedArrayDate(int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = ByteBufferDirect.forSize(size, unit);
  }

  public ArrowFixedArrayDate(ByteBufferDirect buffer, LogicalLayout unit)
      throws UnsupportedMessageException {
    this.size = buffer.capacity() / unit.sizeInBytes();
    this.unit = unit;
    this.buffer = buffer;
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class ReadArrayElement {
    @Specialization(guards = "receiver.getUnit() == Date32")
    static Object doDay(ArrowFixedArrayDate receiver, long index)
        throws UnsupportedMessageException {
      return readDay(receiver.buffer, index);
    }

    @Specialization(guards = "receiver.getUnit() == Date64")
    static Object doMilliseconds(ArrowFixedArrayDate receiver, long index)
        throws UnsupportedMessageException {
      return readMilliseconds(receiver.buffer, index);
    }
  }

  public static Object readDay(ByteBufferDirect buffer, long index)
      throws UnsupportedMessageException {
    if (buffer.isNull((int) index)) {
      return NullValue.get();
    }
    var at = typeAdjustedIndex(index, 4);
    var daysSinceEpoch = buffer.getInt(at);
    var localDate = localDateFromDays(daysSinceEpoch);
    return new ArrowDate(localDate);
  }

  public static Object readMilliseconds(ByteBufferDirect buffer, long index)
      throws UnsupportedMessageException {
    if (buffer.isNull((int) index)) {
      return NullValue.get();
    }
    var at = typeAdjustedIndex(index, 8);
    var secondsPlusNanoSinceEpoch = buffer.getLong(at);
    var seconds = Math.floorDiv(secondsPlusNanoSinceEpoch, nanoDiv);
    var nano = Math.floorMod(secondsPlusNanoSinceEpoch, nanoDiv);
    var zonedDateTime = zonedDateTimeFromSeconds(seconds, nano, utc);
    return new ArrowZonedDateTime(zonedDateTime);
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
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
  private static LocalDate localDateFromDays(int daysSinceEpoch) {
    return LocalDate.ofEpochDay(daysSinceEpoch);
  }

  @CompilerDirectives.TruffleBoundary
  private static ZonedDateTime zonedDateTimeFromSeconds(long seconds, long nano, ZoneId zone) {
    return Instant.ofEpochSecond(seconds, nano).atZone(zone);
  }

  private static final long nanoDiv = 1000000000L;

  private static final ZoneId utc = ZoneId.of("UTC");

  private static int typeAdjustedIndex(long index, int daySizeInBytes) {
    return Math.toIntExact(index * daySizeInBytes);
  }
}
