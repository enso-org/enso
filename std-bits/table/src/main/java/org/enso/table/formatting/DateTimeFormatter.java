package org.enso.table.formatting;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import org.enso.base.time.EnsoDateTimeFormatter;
import org.graalvm.polyglot.Value;

public class DateTimeFormatter implements DataFormatter {
  private final EnsoDateTimeFormatter formatter;

  public DateTimeFormatter(EnsoDateTimeFormatter ensoFormatter) {
    formatter = ensoFormatter;
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
      return formatter.formatLocalDateTime(date);
    }

    if (value instanceof ZonedDateTime date) {
      return formatter.formatZonedDateTime(date);
    }

    throw new IllegalArgumentException("Unsupported type for DateTimeFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof LocalDateTime
        || value instanceof ZonedDateTime
        || (value instanceof Value v && v.isDate() && v.isTime());
  }
}
