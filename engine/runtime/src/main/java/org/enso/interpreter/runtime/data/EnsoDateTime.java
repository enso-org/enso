package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.polyglot.common_utils.Core_Date_Utils;

@ExportLibrary(InteropLibrary.class)
@Builtin(
    pkg = "date",
    name = "DateTime",
    stdlibName = "Standard.Base.Data.Time.Date_Time.Date_Time")
public final class EnsoDateTime extends BuiltinObject {
  private final ZonedDateTime dateTime;

  public EnsoDateTime(ZonedDateTime dateTime) {
    this.dateTime = dateTime;
  }

  @Override
  protected String builtinName() {
    return "Date_Time";
  }

  @Builtin.Method(name = "epoch_start", description = "Return the Enso start of the Epoch")
  public static EnsoDateTime epochStart() {
    return epochStart;
  }

  @Builtin.Method(description = "Return current DateTime")
  @CompilerDirectives.TruffleBoundary
  public static EnsoDateTime now() {
    return new EnsoDateTime(ZonedDateTime.now());
  }

  @Builtin.Method(
      name = "new_builtin",
      description = "Constructs a new Date from a year, month, and day")
  @Builtin.WrapException(from = DateTimeException.class)
  @CompilerDirectives.TruffleBoundary
  public static EnsoDateTime create(
      long year,
      long month,
      long day,
      long hour,
      long minute,
      long second,
      long nanosecond,
      EnsoTimeZone zone) {
    return new EnsoDateTime(
        ZonedDateTime.of(
            Math.toIntExact(year),
            Math.toIntExact(month),
            Math.toIntExact(day),
            Math.toIntExact(hour),
            Math.toIntExact(minute),
            Math.toIntExact(second),
            Math.toIntExact(nanosecond),
            zone.asTimeZone()));
  }

  @Builtin.Method(description = "Gets the year")
  public static long year(EnsoDateTime dt) {
    return dt.year();
  }

  @CompilerDirectives.TruffleBoundary
  public long year() {
    return dateTime.getYear();
  }

  @Builtin.Method(description = "Gets the month")
  public static long month(EnsoDateTime dt) {
    return dt.month();
  }

  @CompilerDirectives.TruffleBoundary
  public long month() {
    return dateTime.getMonthValue();
  }

  @Builtin.Method(description = "Gets the day")
  public static long day(EnsoDateTime dt) {
    return dt.day();
  }

  @CompilerDirectives.TruffleBoundary
  public long day() {
    return dateTime.getDayOfMonth();
  }

  @Builtin.Method(description = "Gets the hour")
  public static long hour(EnsoDateTime dt) {
    return dt.hour();
  }

  @CompilerDirectives.TruffleBoundary
  public long hour() {
    return dateTime.getHour();
  }

  @Builtin.Method(description = "Gets the minute")
  public static long minute(EnsoDateTime dt) {
    return dt.minute();
  }

  @CompilerDirectives.TruffleBoundary
  public long minute() {
    return dateTime.getMinute();
  }

  @Builtin.Method(description = "Gets the second")
  public static long second(EnsoDateTime dt) {
    return dt.second();
  }

  @CompilerDirectives.TruffleBoundary
  public long second() {
    return dateTime.getSecond();
  }

  @Builtin.Method(description = "Gets the millisecond")
  public static long millisecond(EnsoDateTime dt) {
    return dt.millisecond();
  }

  @CompilerDirectives.TruffleBoundary
  public long millisecond() {
    return dateTime.getNano() / 1000_000;
  }

  @Builtin.Method(description = "Gets the microsecond")
  public static long microsecond(EnsoDateTime dt) {
    return dt.microsecond();
  }

  @CompilerDirectives.TruffleBoundary
  public long microsecond() {
    return (dateTime.getNano() / 1000) % 1000;
  }

  @Builtin.Method(name = "nanosecond_builtin", description = "Gets the nanosecond")
  public static long nanosecond(EnsoDateTime dt, boolean includeMilliseconds) {
    return dt.nanosecond(includeMilliseconds);
  }

  @CompilerDirectives.TruffleBoundary
  public long nanosecond(boolean includeMilliseconds) {
    long nanos = dateTime.getNano();
    if (includeMilliseconds) {
      return nanos;
    } else {
      return nanos % 1000;
    }
  }

  @Builtin.Method(name = "zone", description = "Gets the zone")
  public static EnsoTimeZone zone(EnsoDateTime dt) {
    return dt.zone();
  }

  public EnsoTimeZone zone() {
    return new EnsoTimeZone(dateTime.getZone());
  }

  @Builtin.Method(name = "plus_builtin", description = "Adds a duration to this date time")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @CompilerDirectives.TruffleBoundary
  public static EnsoDateTime plus(EnsoDateTime dt, Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDateTime(dt.dateTime.plus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(name = "minus_builtin", description = "Subtracts a duration from this date time")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @CompilerDirectives.TruffleBoundary
  public static EnsoDateTime minus(EnsoDateTime dt, Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDateTime(dt.dateTime.minus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(
      name = "time_of_day",
      description = "Return the localtime of this date time value.")
  @CompilerDirectives.TruffleBoundary
  public static EnsoTimeOfDay toLocalTime(EnsoDateTime dt) {
    return new EnsoTimeOfDay(dt.dateTime.toLocalTime());
  }

  @Builtin.Method(name = "date", description = "Return the localdate of this date time value.")
  @CompilerDirectives.TruffleBoundary
  public static EnsoDate toLocalDate(EnsoDateTime dt) {
    return new EnsoDate(dt.dateTime.toLocalDate());
  }

  @Builtin.Method(description = "Return a text representation of this date-time.")
  @CompilerDirectives.TruffleBoundary
  public static Text toText(EnsoDateTime dt) {
    return Text.create(Core_Date_Utils.defaultZonedDateTimeFormatter.format(dt.dateTime));
  }

  @ExportMessage
  boolean isDate() {
    return true;
  }

  @ExportMessage
  LocalDate asDate() {
    return dateTime.toLocalDate();
  }

  @ExportMessage
  boolean isTime() {
    return true;
  }

  @ExportMessage
  LocalTime asTime() {
    return dateTime.toLocalTime();
  }

  @ExportMessage
  boolean isTimeZone() {
    return true;
  }

  @ExportMessage
  ZoneId asTimeZone() {
    return dateTime.getZone();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  @Override
  public Object toDisplayString(boolean allowSideEffects) {
    return Core_Date_Utils.defaultZonedDateTimeFormatter.format(dateTime);
  }

  // 15. October 1582
  /** 15. October 1582 in UTC timezone. Note that Java considers an epoch start 1.1.1970 UTC. */
  private static final EnsoDateTime epochStart =
      EnsoDateTime.create(1582, 10, 15, 0, 0, 0, 0, EnsoTimeZone.parse("UTC"));
}
