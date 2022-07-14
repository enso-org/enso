package org.enso.base;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Period;
import java.time.ZonedDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.WeekFields;
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

  public static String local_date_to_text(LocalDate date) {
    return default_date_formatter().format(date);
  }

  public static String local_date_to_text(Object any) {
    return any == null ? "No instance of date" : "Instance of date " + any.getClass();
  }

  public static String local_date_format(LocalDate date, Object format) {
    return DateTimeFormatter.ofPattern(format.toString()).format(date);
  }

  public static ZonedDateTime date_with_time(LocalDate date, LocalTime time, ZoneId zone) {
    return date.atTime(time).atZone(zone);
  }

  public static LocalDate date_adjust(LocalDate date, long add, Period duration) {
    return add == 1 ? date.plus(duration) : date.minus(duration);
  }

  public static long week_of_year(LocalDate date, Locale locale) {
    return WeekFields.of(locale).weekOfYear().getFrom(date);
  }

  public static int compare_to(LocalDate self, LocalDate that) {
    return self.compareTo(that);
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
  public static ZonedDateTime parse_time(String text) {
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
   * @return parsed ZonedDateTime instance.
   */
  public static ZonedDateTime parse_time_format(String text, String pattern, Locale locale) {
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
}
