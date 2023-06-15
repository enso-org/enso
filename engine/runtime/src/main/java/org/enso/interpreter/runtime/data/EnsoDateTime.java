package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(
    pkg = "date",
    name = "DateTime",
    stdlibName = "Standard.Base.Data.Time.Date_Time.Date_Time")
public final class EnsoDateTime implements TruffleObject {
  private final ZonedDateTime dateTime;

  public EnsoDateTime(ZonedDateTime dateTime) {
    this.dateTime = dateTime;
  }

  @Builtin.Method(
      name = "epoch_start",
      description = "Return the Enso start of the Epoch",
      autoRegister = false)
  public static EnsoDateTime epochStart() {
    return epochStart;
  }

  @Builtin.Method(description = "Return current DateTime", autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static EnsoDateTime now() {
    return new EnsoDateTime(ZonedDateTime.now());
  }

  @Builtin.Method(
      name = "new_builtin",
      description = "Constructs a new Date from a year, month, and day",
      autoRegister = false)
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
  @CompilerDirectives.TruffleBoundary
  public long year() {
    return dateTime.getYear();
  }

  @Builtin.Method(description = "Gets the month")
  @CompilerDirectives.TruffleBoundary
  public long month() {
    return dateTime.getMonthValue();
  }

  @Builtin.Method(description = "Gets the day")
  @CompilerDirectives.TruffleBoundary
  public long day() {
    return dateTime.getDayOfMonth();
  }

  @Builtin.Method(description = "Gets the hour")
  @CompilerDirectives.TruffleBoundary
  public long hour() {
    return dateTime.getHour();
  }

  @Builtin.Method(description = "Gets the minute")
  @CompilerDirectives.TruffleBoundary
  public long minute() {
    return dateTime.getMinute();
  }

  @Builtin.Method(description = "Gets the second")
  @CompilerDirectives.TruffleBoundary
  public long second() {
    return dateTime.getSecond();
  }

  @Builtin.Method(description = "Gets the nanosecond")
  @CompilerDirectives.TruffleBoundary
  public long nanosecond() {
    return dateTime.getNano();
  }

  @Builtin.Method(name = "zone", description = "Gets the zone")
  public EnsoTimeZone zone() {
    return new EnsoTimeZone(dateTime.getZone());
  }

  @Builtin.Method(name = "plus_builtin", description = "Adds a duration to this date time")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoDateTime plus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDateTime(dateTime.plus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(name = "minus_builtin", description = "Subtracts a duration from this date time")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @CompilerDirectives.TruffleBoundary
  public EnsoDateTime minus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDateTime(dateTime.minus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(
      name = "time_of_day",
      description = "Return the localtime of this date time value.")
  @CompilerDirectives.TruffleBoundary
  public EnsoTimeOfDay toLocalTime() {
    return new EnsoTimeOfDay(dateTime.toLocalTime());
  }

  @Builtin.Method(name = "date", description = "Return the localdate of this date time value.")
  @CompilerDirectives.TruffleBoundary
  public EnsoDate toLocalDate() {
    return new EnsoDate(dateTime.toLocalDate());
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  @CompilerDirectives.TruffleBoundary
  public Text toText() {
    return Text.create(DateTimeFormatter.ISO_ZONED_DATE_TIME.format(dateTime));
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
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().dateTime();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().dateTime();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  public Object toDisplayString(boolean allowSideEffects) {
    return DateTimeFormatter.ISO_ZONED_DATE_TIME.format(dateTime);
  }

  // 15. October 1582
  /** 15. October 1582 in UTC timezone. Note that Java considers an epoch start 1.1.1970 UTC. */
  private static final EnsoDateTime epochStart =
      EnsoDateTime.create(1582, 10, 15, 0, 0, 0, 0, EnsoTimeZone.parse("UTC"));
}
