package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.TimeOfDayBuilder;

import java.time.LocalTime;
import java.util.Locale;

public class TimeOfDayParser extends BaseTimeParser {
  public TimeOfDayParser(String[] formats, Locale locale) {
    super(formats, locale, LocalTime::parse);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new TimeOfDayBuilder(capacity);
  }
}
