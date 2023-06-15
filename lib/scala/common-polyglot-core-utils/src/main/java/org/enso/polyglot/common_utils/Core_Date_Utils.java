package org.enso.polyglot.common_utils;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;
import java.util.Locale;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

public class Core_Date_Utils {
  /**
   * Replace space with T in ISO date time string to make it compatible with ISO format.
   * @param dateString Raw date time string with either space or T as separator
   * @return ISO format date time string
   */
  public static String normaliseISODateTime(String dateString) {
    if (dateString != null && dateString.length() > 10 && dateString.charAt(10) == ' ') {
      var builder = new StringBuilder(dateString);
      builder.replace(10, 11, "T");
      return builder.toString();
    }

    return dateString;
  }

  /** @return default Date Time formatter for parsing a Date_Time. */
  public static DateTimeFormatter defaultZonedDateTimeFormatter() {
    return new DateTimeFormatterBuilder().append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        .optionalStart().parseLenient().appendOffsetId().optionalEnd()
        .optionalStart().appendLiteral('[').parseCaseSensitive().appendZoneRegionId().appendLiteral(']')
        .toFormatter();
  }

  /** @return default Date formatter for parsing a Date. */
  public static DateTimeFormatter defaultLocalDateFormatter() {
    return DateTimeFormatter.ISO_LOCAL_DATE;
  }

  /** @return default Time formatter for parsing a Time_Of_Day. */
  public static DateTimeFormatter defaultLocalTimeFormatter() {
    return DateTimeFormatter.ISO_LOCAL_TIME;
  }

  /**
   * Parse a date string into a LocalDate.
   * Allows missing day (assumes first day of month) or missing year (assumes current year).
   *
   * @param dateString the date time string
   * @param formatter the formatter to use
   * @return the parsed date time
   */
  public static LocalDate parseLocalDate(String dateString, DateTimeFormatter formatter) {
    var parsed = formatter.parse(dateString);

    if (parsed.isSupported(ChronoField.EPOCH_DAY)) {
      return LocalDate.ofEpochDay(parsed.getLong(ChronoField.EPOCH_DAY));
    }

    // Allow Year and Month to be parsed without a day (use first day of month).
    if (parsed.isSupported(ChronoField.YEAR) && parsed.isSupported(ChronoField.MONTH_OF_YEAR)) {
      var dayOfMonth = parsed.isSupported(ChronoField.DAY_OF_MONTH)
          ? parsed.get(ChronoField.DAY_OF_MONTH)
          : 1;
      return LocalDate.of(parsed.get(ChronoField.YEAR), parsed.get(ChronoField.MONTH_OF_YEAR), dayOfMonth);
    }

    // Allow Month and Day to be parsed without a year (use current year).
    if (parsed.isSupported(ChronoField.DAY_OF_MONTH) && parsed.isSupported(ChronoField.MONTH_OF_YEAR)) {
      return LocalDate.of(LocalDate.now().getYear(), parsed.get(ChronoField.MONTH_OF_YEAR), parsed.get(ChronoField.DAY_OF_MONTH));
    }

    throw new DateTimeParseException("Unable to parse date.", dateString, 0);

  }

  /**
   * Parse a date time string into a ZonedDateTime.
   *
   * @param dateString the date time string
   * @param formatter the formatter to use
   * @return the parsed date time
   */
  public static ZonedDateTime parseZonedDateTime(String dateString, DateTimeFormatter formatter) {
    var resolved = formatter.parse(dateString);

    try {
      // Resolve Zone
      var zone = resolved.query(TemporalQueries.zoneId());
      zone = zone != null ? zone :
              (resolved.isSupported(ChronoField.OFFSET_SECONDS)
                      ? ZoneOffset.ofTotalSeconds(resolved.get(ChronoField.OFFSET_SECONDS))
                      : ZoneId.systemDefault());

      // Instant Based
      if (resolved.isSupported(INSTANT_SECONDS)) {
        long epochSecond = resolved.getLong(INSTANT_SECONDS);
        int nanoOfSecond = resolved.get(NANO_OF_SECOND);
        return ZonedDateTime.ofInstant(java.time.Instant.ofEpochSecond(epochSecond, nanoOfSecond), zone);
      }

      // Local Based
      var localDate = LocalDate.from(resolved);
      var localTime = LocalTime.from(resolved);
      return ZonedDateTime.of(localDate, localTime, zone);
    } catch (DateTimeException e) {
      throw new DateTimeException("Unable to parse Text '" + dateString + "' to Date_Time.", e);
    } catch (ArithmeticException e) {
      throw new DateTimeException("Unable to parse Text '" + dateString + "' to Date_Time due to arithmetic error.", e);
    }
  }

  /**
   * Creates a DateTimeFormatter from a format string, supporting building standard formats.
   *
   * @param format format string
   * @param locale locale needed for custom formats
   * @return DateTimeFormatter
   */
  public static DateTimeFormatter make_formatter(String format, Locale locale) {
    var usedLocale = locale == Locale.ROOT ? Locale.US : locale;
    return switch (format) {
      case "ENSO_ZONED_DATE_TIME" -> defaultZonedDateTimeFormatter();
      case "ISO_ZONED_DATE_TIME" -> DateTimeFormatter.ISO_ZONED_DATE_TIME;
      case "ISO_OFFSET_DATE_TIME" -> DateTimeFormatter.ISO_OFFSET_DATE_TIME;
      case "ISO_LOCAL_DATE_TIME" -> DateTimeFormatter.ISO_LOCAL_DATE_TIME;
      case "ISO_LOCAL_DATE" -> DateTimeFormatter.ISO_LOCAL_DATE;
      case "ISO_LOCAL_TIME" -> DateTimeFormatter.ISO_LOCAL_TIME;
      default -> DateTimeFormatter.ofPattern(format, usedLocale);
    };
  }
}
