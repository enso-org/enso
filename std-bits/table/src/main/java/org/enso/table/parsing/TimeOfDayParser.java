package org.enso.table.parsing;

import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.TimeOfDayBuilder;

public class TimeOfDayParser extends BaseTimeParser {
  public TimeOfDayParser(EnsoDateTimeFormatter[] formatters) {
    super(
        formatters,
        (String text, EnsoDateTimeFormatter formatter) -> formatter.parseLocalTime(text));
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new TimeOfDayBuilder(capacity);
  }
}
