package org.enso.polyglot.common_utils;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;

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
}
