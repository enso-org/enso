package org.enso.table.parsing;

import java.util.Locale;
import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.DateTimeBuilder;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(String[] formats, Locale locale) {
    super(formats, locale, Core_Date_Utils::parseZonedDateTime);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
