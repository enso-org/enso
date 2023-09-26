package org.enso.table.formatting;

import org.enso.base.time.EnsoDateTimeFormatter;
import org.graalvm.polyglot.Value;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateFormatter implements DataFormatter {
  private final DateTimeFormatter formatter;

  public DateFormatter(EnsoDateTimeFormatter ensoFormatter) {
    formatter = ensoFormatter.getRawJavaFormatter();
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Value v && v.isDate()) {
      value = v.asDate();
    }

    if (value instanceof LocalDate date) {
      return date.format(formatter);
    }

    throw new IllegalArgumentException("Unsupported type for DateFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof LocalDate || (value instanceof Value v && v.isDate() && !v.isTime());
  }
}
