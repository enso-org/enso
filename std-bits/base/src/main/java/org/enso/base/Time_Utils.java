package org.enso.base;

import org.enso.base.time.Date_Time_Utils;
import org.enso.base.time.Date_Utils;
import org.enso.base.time.TimeUtilsBase;
import org.enso.base.time.Time_Of_Day_Utils;
import org.graalvm.polyglot.Value;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;
import java.time.temporal.WeekFields;
import java.util.Locale;

/** Utils for standard library operations on Time. */
public class Time_Utils {
  public enum AdjustOp {
    PLUS,
    MINUS
  }

  /**
   * Creates a DateTimeFormatter from a format string, supporting building standard formats.
   * @param format format string
   * @param locale locale needed for custom formats
   * @return DateTimeFormatter
   */
  public static DateTimeFormatter make_formatter(String format, Locale locale) {
    return switch (format) {
      case "ENSO_ZONED_DATE_TIME" -> Time_Utils.default_zoned_date_time_formatter();
      case "ISO_ZONED_DATE_TIME" -> DateTimeFormatter.ISO_ZONED_DATE_TIME;
      case "ISO_OFFSET_DATE_TIME" -> DateTimeFormatter.ISO_OFFSET_DATE_TIME;
      case "ISO_LOCAL_DATE_TIME" -> DateTimeFormatter.ISO_LOCAL_DATE_TIME;
      case "ISO_LOCAL_DATE" -> DateTimeFormatter.ISO_LOCAL_DATE;
      case "ISO_LOCAL_TIME" -> DateTimeFormatter.ISO_LOCAL_TIME;
      default -> DateTimeFormatter.ofPattern(format, locale).withZone(ZoneId.systemDefault());
    };
  }

  /** @return default Date Time formatter. */
  public static DateTimeFormatter default_zoned_date_time_formatter() {
    return new DateTimeFormatterBuilder().append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            .optionalStart().parseLenient().appendOffsetId().optionalEnd()
            .optionalStart().appendLiteral('[').parseCaseSensitive().appendZoneRegionId().appendLiteral(']')
            .toFormatter().withZone(ZoneId.systemDefault());
  }

  public static String local_date_format(LocalDate date, Object format) {
    return DateTimeFormatter.ofPattern(format.toString()).format(date);
  }

  public static LocalDate date_adjust(LocalDate date, AdjustOp op, Period period) {
    return switch (op) {
      case PLUS -> date.plus(period);
      case MINUS -> date.minus(period);
    };
  }

  public static ZonedDateTime datetime_adjust(ZonedDateTime datetime, AdjustOp op, Period period) {
    return switch (op) {
      case PLUS -> datetime.plus(period);
      case MINUS -> datetime.minus(period);
    };
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
  public static ZonedDateTime parse_datetime(String text, String pattern, Locale locale) {
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
   * in Enso; <a href="https://www.pivotaltracker.com/story/show/183219169">Pivotal issue.</a>
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
