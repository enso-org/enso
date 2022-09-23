package org.enso.base;

import org.enso.base.time.Date_Time_Utils;
import org.enso.base.time.Date_Utils;
import org.enso.base.time.TimeUtilsBase;
import org.enso.base.time.Time_Of_Day_Utils;
import org.graalvm.polyglot.Value;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.*;
import java.util.Locale;

/** Utils for standard library operations on Time. */
public class Time_Utils {

  /**
   * The ISO-like date-time formatter that formats or parses a date-time with optional offset and
   * zone, such as '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  public static final DateTimeFormatter TIME_FORMAT;

  static {
    TIME_FORMAT =
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

  public enum AdjustOp {
    PLUS,
    MINUS
  }

  /** @return default Time formatter. */
  public static DateTimeFormatter default_time_formatter() {
    return DateTimeFormatter.ISO_ZONED_DATE_TIME;
  }

  /** @return default Date formatter. */
  public static DateTimeFormatter default_date_formatter() {
    return DateTimeFormatter.ISO_LOCAL_DATE;
  }

  /** @return default Time_Of_Day formatter. */
  public static DateTimeFormatter default_time_of_day_formatter() {
    return DateTimeFormatter.ISO_LOCAL_TIME;
  }

  public static String local_date_format(LocalDate date, Object format) {
    return DateTimeFormatter.ofPattern(format.toString()).format(date);
  }

  public static ZonedDateTime date_with_time(LocalDate date, LocalTime time, ZoneId zone) {
    return date.atTime(time).atZone(zone);
  }

  public static LocalDate date_adjust(LocalDate date, AdjustOp op, Period duration) {
    switch (op) {
      case PLUS:
        return date.plus(duration);
      case MINUS:
        return date.minus(duration);
      default:
        throw new DateTimeException("Unknown adjust operation");
    }
  }

  public static ZonedDateTime datetime_adjust(
      ZonedDateTime datetime, AdjustOp op, Period period, Duration duration) {
    switch (op) {
      case PLUS:
        return datetime.plus(period).plus(duration);
      case MINUS:
        return datetime.minus(period).minus(duration);
      default:
        throw new DateTimeException("Unknown adjust operation");
    }
  }

  public static LocalTime time_adjust(LocalTime time, AdjustOp op, Duration duration) {
    switch (op) {
      case PLUS:
        return time.plus(duration);
      case MINUS:
        return time.minus(duration);
      default:
        throw new DateTimeException("Unknown adjust operation");
    }
  }

  public static int get_field_as_localdate(LocalDate date, TemporalField field) {
    return date.get(field);
  }

  public static int get_field_as_zoneddatetime(ZonedDateTime date, TemporalField field) {
    return date.get(field);
  }

  public static boolean is_leap_year(LocalDate date) {
    return date.isLeapYear();
  }

  public static int length_of_month(LocalDate date) {
    return date.lengthOfMonth();
  }

  public static long week_of_year_localdate(LocalDate date, Locale locale) {
    return WeekFields.of(locale).weekOfYear().getFrom(date);
  }

  public static long week_of_year_zoneddatetime(ZonedDateTime date, Locale locale) {
    return WeekFields.of(locale).weekOfYear().getFrom(date);
  }

  public static Duration duration_between(
      ZonedDateTime start, ZonedDateTime end, boolean timezoneAware) {
    return timezoneAware
        ? Duration.between(start, end)
        : Duration.between(start.toLocalDateTime(), end.toLocalDateTime());
  }

  public static int compare_to_localdate(LocalDate self, LocalDate that) {
    return self.compareTo(that);
  }

  public static int compare_to_zoneddatetime(ZonedDateTime self, ZonedDateTime that) {
    return self.compareTo(that);
  }

  public static int compare_to_localtime(LocalTime self, LocalTime that) {
    return self.compareTo(that);
  }

  public static boolean equals_zone(ZoneId self, ZoneId that) {
    return self.equals(that);
  }

  /**
   * Obtains an instance of ZonedDateTime from a text string.
   *
   * <p>Accepts:
   *
   * <ul>
   *   <li>Local date time, such as '2011-12-03T10:15:30' adding system dafault timezone.
   *   <li>Offset date time, such as '2011-12-03T10:15:30+01:00' parsing offset as a timezone.
   *   <li>Zoned date time, such as '2011-12-03T10:15:30+01:00[Europe/Paris]' with optional region
   *       id in square brackets.
   * </ul>
   *
   * @param text the string to parse.
   * @return parsed ZonedDateTime instance.
   */
  public static ZonedDateTime parse_datetime(String text) {
    TemporalAccessor time = TIME_FORMAT.parseBest(text, ZonedDateTime::from, LocalDateTime::from);
    if (time instanceof ZonedDateTime) {
      return (ZonedDateTime) time;
    } else if (time instanceof LocalDateTime) {
      return ((LocalDateTime) time).atZone(ZoneId.systemDefault());
    }
    throw new DateTimeException("Text '" + text + "' could not be parsed as Time.");
  }

  /**
   * Obtains an instance of ZonedDateTime from text using custom format string.
   *
   * <p>First tries to parse text as ZonedDateTime, then falls back to parsing LocalDateTime adding
   * system default timezone.
   *
   * @param text the string to parse.
   * @param pattern the format string.
   * @param locale localization config to be uses in the formatter.
   * @return parsed ZonedDateTime instance.
   */
  public static ZonedDateTime parse_datetime_format(String text, String pattern, Locale locale) {
    TemporalAccessor time =
        DateTimeFormatter.ofPattern(pattern)
            .withLocale(locale)
            .parseBest(text, ZonedDateTime::from, LocalDateTime::from);
    if (time instanceof ZonedDateTime) {
      return (ZonedDateTime) time;
    } else if (time instanceof LocalDateTime) {
      return ((LocalDateTime) time).atZone(ZoneId.systemDefault());
    }
    throw new DateTimeException("Text '" + text + "' could not be parsed as Time.");
  }

  /**
   * Obtains an instance of LocalTime from a text string using a custom string.
   *
   * @param text the string to parse.
   * @param pattern the format string.
   * @param locale localization config to be uses in the formatter.
   * @return parsed LocalTime instance.
   */
  public static LocalTime parse_time(String text, String pattern, Locale locale) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);
    return (LocalTime.parse(text, formatter.withLocale(locale)));
  }

  /**
   * Normally this method could be done in Enso by pattern matching, but currently matching on Time
   * types is not supported, so this is a workaround.
   *
   * <p>TODO once the related issue is fixed, this workaround may be replaced with pattern matching
   * in Enso; the related Pivotal issue: https://www.pivotaltracker.com/story/show/183219169
   */
  public static TimeUtilsBase utils_for(Value value) {
    boolean isDate = value.isDate();
    boolean isTime = value.isTime();
    if (isDate && isTime) return Date_Time_Utils.INSTANCE;
    if (isDate) return Date_Utils.INSTANCE;
    if (isTime) return Time_Of_Day_Utils.INSTANCE;
    throw new IllegalArgumentException("Unexpected argument type: " + value);
  }

  public static ZoneOffset get_datetime_offset(ZonedDateTime datetime) {
    return datetime.getOffset();
  }

  /**
   * Counts days within the range from start (inclusive) to end (exclusive).
   *
   * <p>If start is before end, it will return 0.
   */
  public static long days_between(LocalDate start, LocalDate end) {
    return ChronoUnit.DAYS.between(start, end);
  }
}
