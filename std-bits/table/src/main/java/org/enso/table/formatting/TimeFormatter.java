package org.enso.table.formatting;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import org.enso.base.time.EnsoDateTimeFormatter;
import org.graalvm.polyglot.Value;

public class TimeFormatter implements DataFormatter {
  private final DateTimeFormatter formatter;

  public TimeFormatter(EnsoDateTimeFormatter ensoFormatter) {
    formatter = ensoFormatter.getRawJavaFormatter();
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Value v && v.isTime()) {
      value = v.asTime();
    }

    if (value instanceof LocalTime date) {
      return date.format(formatter);
    }

    throw new IllegalArgumentException("Unsupported type for TimeFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof LocalTime || (value instanceof Value v && !v.isDate() && v.isTime());
  }
}
