package org.enso.table.parsing;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateBuilder;

import java.util.Locale;

public class DateParser extends BaseTimeParser {
  public DateParser(String[] formats, Locale locale) {
    super(formats, locale, Core_Date_Utils::parseLocalDate);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateBuilder(capacity);
  }
}
