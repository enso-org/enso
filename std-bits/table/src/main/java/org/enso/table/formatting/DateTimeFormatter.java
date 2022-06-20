package org.enso.table.formatting;

import java.time.LocalDateTime;
import java.util.Locale;

public class DateTimeFormatter implements DataFormatter {
  private final java.time.format.DateTimeFormatter formatter;

  public DateTimeFormatter(String formatString, Locale locale) {
    formatter = java.time.format.DateTimeFormatter.ofPattern(formatString, locale);
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof LocalDateTime date) {
      return date.format(formatter);
    }

    throw new IllegalArgumentException("Unsupported type for DateTimeFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof LocalDateTime;
  }
}
