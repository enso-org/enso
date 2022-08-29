package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateTimeBuilder;

import java.time.ZonedDateTime;
import java.util.Locale;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(String[] formats, Locale locale) {
    super(formats, locale, ZonedDateTime::parse);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
