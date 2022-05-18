package org.enso.table.parsing;

import java.time.LocalDateTime;
import java.util.Locale;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(String[] formats, Locale locale) {
    super(formats, locale, LocalDateTime::parse);
  }
}
