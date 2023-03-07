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
    var datetime = formatter.parseBest(text, ZonedDateTime::from, LocalDateTime::from);
    if (datetime instanceof ZonedDateTime zdt) {
      return zdt;
    } else if (datetime instanceof LocalDateTime ldt) {
      return ldt.atZone(ZoneId.systemDefault());
    }
    throw new DateTimeParseException("Invalid date time", text, 0);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
