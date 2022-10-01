package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateTimeBuilder;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(String[] formats, Locale locale) {
    super(formats, locale, DateTimeParser::parse);
  }

  private static ZonedDateTime parse(String text, DateTimeFormatter formatter)
      throws DateTimeParseException {
    try {
      return ZonedDateTime.parse(text, formatter);
    } catch (DateTimeParseException ignored) {
      return LocalDateTime.parse(text, formatter).atZone(ZoneId.systemDefault());
    }
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
