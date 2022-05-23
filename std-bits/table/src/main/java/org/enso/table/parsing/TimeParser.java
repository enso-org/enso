package org.enso.table.parsing;

import java.time.LocalTime;
import java.util.Locale;

public class TimeParser extends BaseTimeParser {
  public TimeParser(String[] formats, Locale locale) {
    super(formats, locale, LocalTime::parse);
  }
}
