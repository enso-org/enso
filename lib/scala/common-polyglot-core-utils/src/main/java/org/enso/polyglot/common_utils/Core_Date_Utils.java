package org.enso.polyglot.common_utils;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;

public class Core_Date_Utils {
  /** default Date Time formatter for parsing a Date_Time. */
  public static final DateTimeFormatter defaultZonedDateTimeFormatter =
      new DateTimeFormatterBuilder()
          .parseLenient()
          .append(DateTimeFormatter.ISO_LOCAL_DATE)
          .appendLiteral(' ')
          .append(DateTimeFormatter.ISO_LOCAL_TIME)
          .optionalStart()
          .parseLenient()
          .appendOffsetId()
          .optionalEnd()
          .optionalStart()
          .appendLiteral('[')
          .parseCaseSensitive()
          .appendZoneRegionId()
          .appendLiteral(']')
          .toFormatter();

  /** default Date formatter for parsing a Date. */
  public static final DateTimeFormatter defaultLocalDateFormatter =
      DateTimeFormatter.ISO_LOCAL_DATE;

  /** default Time formatter for parsing a Time_Of_Day. */
  public static final DateTimeFormatter defaultLocalTimeFormatter =
      DateTimeFormatter.ISO_LOCAL_TIME;

  /**
   * Parse a date string into a LocalDate. Allows missing day (assumes first day of month) or
   * missing year (assumes current year).
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
      var dayOfMonth =
          parsed.isSupported(ChronoField.DAY_OF_MONTH) ? parsed.get(ChronoField.DAY_OF_MONTH) : 1;
      return LocalDate.of(
          parsed.get(ChronoField.YEAR), parsed.get(ChronoField.MONTH_OF_YEAR), dayOfMonth);
    }

    // Allow Month and Day to be parsed without a year (use current year).
    if (parsed.isSupported(ChronoField.DAY_OF_MONTH)
        && parsed.isSupported(ChronoField.MONTH_OF_YEAR)) {
      return LocalDate.of(
          LocalDate.now().getYear(),
          parsed.get(ChronoField.MONTH_OF_YEAR),
          parsed.get(ChronoField.DAY_OF_MONTH));
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
      zone =
          zone != null
              ? zone
              : (resolved.isSupported(ChronoField.OFFSET_SECONDS)
                  ? ZoneOffset.ofTotalSeconds(resolved.get(ChronoField.OFFSET_SECONDS))
                  : ZoneId.systemDefault());

      // Instant Based
      if (resolved.isSupported(INSTANT_SECONDS)) {
        long epochSecond = resolved.getLong(INSTANT_SECONDS);
        int nanoOfSecond = resolved.get(NANO_OF_SECOND);
        return ZonedDateTime.ofInstant(
            java.time.Instant.ofEpochSecond(epochSecond, nanoOfSecond), zone);
      }

      // Local Based
      var localDate = LocalDate.from(resolved);
      var localTime = LocalTime.from(resolved);
      return ZonedDateTime.of(localDate, localTime, zone);
    } catch (DateTimeException e) {
      throw new DateTimeException("Unable to parse Text '" + dateString + "' to Date_Time.", e);
    } catch (ArithmeticException e) {
      throw new DateTimeException(
          "Unable to parse Text '" + dateString + "' to Date_Time due to arithmetic error.", e);
    }
  }
}
