package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.TemporalAccessor;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "date", name = "DateTime", stdlibName = "Standard.Base.Data.Time.Date_Time")
public final class EnsoDateTime implements TruffleObject {
  private final ZonedDateTime dateTime;

  public EnsoDateTime(ZonedDateTime dateTime) {
    this.dateTime = dateTime;
  }

  @Builtin.Method(description = "Return current DateTime")
  public static EnsoDateTime now() {
    return new EnsoDateTime(ZonedDateTime.now());
  }

  /**
   * Obtains an instance of EnsoDateTime (ZonedDateTime) from a text string.
   *
   * <p>Accepts:
   *
   * <ul>
   *   <li>Local date time, such as '2011-12-03T10:15:30' adding system default timezone.
   *   <li>Offset date time, such as '2011-12-03T10:15:30+01:00' parsing offset as a timezone.
   *   <li>Zoned date time, such as '2011-12-03T10:15:30+01:00[Europe/Paris]' with optional region
   *       id in square brackets.
   * </ul>
   *
   * @param text the string to parse.
   * @return parsed ZonedDateTime instance wrapped in EnsoDateTime.
   */
  @Builtin.Method(
      name = "parse_builtin",
      description = "Constructs a new DateTime from text with optional pattern")
  @Builtin.Specialize
  @Builtin.WrapException(from = DateTimeParseException.class, to = PolyglotError.class)
  public static EnsoDateTime parse(String text) {
    TemporalAccessor time = TIME_FORMAT.parseBest(text, ZonedDateTime::from, LocalDateTime::from);
    if (time instanceof ZonedDateTime) {
      return new EnsoDateTime((ZonedDateTime) time);
    } else if (time instanceof LocalDateTime) {
      return new EnsoDateTime(((LocalDateTime) time).atZone(ZoneId.systemDefault()));
    }
    throw new DateTimeException("Text '" + text + "' could not be parsed as Time.");
  }

  @Builtin.Method(
      name = "new_builtin",
      description = "Constructs a new Date from a year, month, and day")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class)
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

  @Builtin.Method(description = "Return the number of seconds from the Unix epoch.")
  @CompilerDirectives.TruffleBoundary
  public long toEpochSeconds() {
    return dateTime.toEpochSecond();
  }

  @Builtin.Method(description = "Return the number of milliseconds from the Unix epoch.")
  @CompilerDirectives.TruffleBoundary
  public long toEpochMilliseconds() {
    return dateTime.toInstant().toEpochMilli();
  }

  @Builtin.Method(
      name = "to_localtime_builtin",
      description = "Return the localtime of this date time value.")
  public EnsoTimeOfDay toLocalTime() {
    return new EnsoTimeOfDay(dateTime.toLocalTime());
  }

  @Builtin.Method(
      name = "to_localdate_builtin",
      description = "Return the localdate of this date time value.")
  public EnsoDate toLocalDate() {
    return new EnsoDate(dateTime.toLocalDate());
  }

  @Builtin.Method(description = "Return this datetime in the provided time zone.")
  public EnsoDateTime atZone(EnsoTimeZone zone) {
    return new EnsoDateTime(dateTime.withZoneSameInstant(zone.asTimeZone()));
  }

  @Builtin.Method(
      name = "to_time_builtin",
      description = "Combine this day with time to create a point in time.")
  public EnsoDateTime toTime(EnsoTimeOfDay timeOfDay, EnsoTimeZone zone) {
    return new EnsoDateTime(
        dateTime.toLocalDate().atTime(timeOfDay.asTime()).atZone(zone.asTimeZone()));
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  public Text toText() {
    return Text.create(DateTimeFormatter.ISO_ZONED_DATE_TIME.format(dateTime));
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  @Builtin.Specialize
  public Text format(String pattern) {
    return Text.create(DateTimeFormatter.ofPattern(pattern).format(dateTime));
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
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().dateTime();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  public final Object toDisplayString(boolean allowSideEffects) {
    return DateTimeFormatter.ISO_ZONED_DATE_TIME.format(dateTime);
  }

  private static final DateTimeFormatter TIME_FORMAT =
      new DateTimeFormatterBuilder()
          .parseCaseInsensitive()
          .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
          .parseLenient()
          .optionalStart()
          .appendZoneOrOffsetId()
          .optionalEnd()
          .parseStrict()
          .optionalStart()
          .appendLiteral('[')
          .parseCaseSensitive()
          .appendZoneRegionId()
          .appendLiteral(']')
          .optionalEnd()
          .toFormatter();
}
