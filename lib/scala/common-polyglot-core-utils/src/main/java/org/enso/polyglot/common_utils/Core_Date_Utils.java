package org.enso.polyglot.common_utils;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;

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
}
