package org.enso.table.parsing;

import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.DateTimeBuilder;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(EnsoDateTimeFormatter[] formatters) {
    super(
        formatters,
        (String text, EnsoDateTimeFormatter formatter) -> formatter.parseZonedDateTime(text));
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
