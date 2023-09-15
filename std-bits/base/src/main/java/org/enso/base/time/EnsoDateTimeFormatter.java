package org.enso.base.time;

import java.time.format.DateTimeFormatter;

public class EnsoDateTimeFormatter {
  private final DateTimeFormatter formatter;
  private final boolean needsISOTreplaceWorkaround;

  private EnsoDateTimeFormatter(DateTimeFormatter formatter, boolean needsISOreplaceTWorkaround) {
    this.formatter = formatter;
    this.needsISOTreplaceWorkaround = needsISOreplaceTWorkaround;
  }
}
