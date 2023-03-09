package org.enso.table.formatting;

import org.enso.base.Time_Utils;
import org.graalvm.polyglot.Value;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Locale;

public class DateTimeFormatter implements DataFormatter {
  private final java.time.format.DateTimeFormatter formatter;

  public DateTimeFormatter(String formatString, Locale locale) {
    formatter = Time_Utils.make_output_formatter(formatString, locale);
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Value v && v.isDate() && v.isTime()) {
      value = v.asDate().atTime(v.asTime());
      if (v.isTimeZone()) {
        value = ((LocalDateTime) value).atZone(v.asTimeZone());
      }
    }

    if (value instanceof LocalDateTime date) {
      return date.format(formatter);
    }

    if (value instanceof ZonedDateTime date) {
      return date.format(formatter);
    }

    throw new IllegalArgumentException("Unsupported type for DateTimeFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof LocalDateTime || value instanceof ZonedDateTime || (value instanceof Value v && v.isDate() && v.isTime());
  }
}
