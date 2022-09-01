package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateBuilder;

import java.time.LocalDate;
import java.util.Locale;

public class DateParser extends BaseTimeParser {
  public DateParser(String[] formats, Locale locale) {
    super(formats, locale, LocalDate::parse);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateBuilder(capacity);
  }
}
