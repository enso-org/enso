package org.enso.table.parsing;

import java.time.LocalDate;
import java.util.Locale;

public class DateParser extends BaseTimeParser {
  public DateParser(String[] formats, Locale locale) {
    super(formats, locale, LocalDate::parse);
  }
}
