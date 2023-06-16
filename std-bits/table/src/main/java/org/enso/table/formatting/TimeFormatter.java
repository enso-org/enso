package org.enso.table.formatting;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.graalvm.polyglot.Value;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class TimeFormatter implements DataFormatter {
  private final DateTimeFormatter formatter;

  public TimeFormatter(String formatString, Locale locale) {
    formatter = Core_Date_Utils.make_formatter(formatString, locale);
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
