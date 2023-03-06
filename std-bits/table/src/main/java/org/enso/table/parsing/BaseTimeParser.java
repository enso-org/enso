package org.enso.table.parsing;

import org.enso.base.Time_Utils;
import org.enso.table.parsing.problems.ProblemAggregator;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;

public abstract class BaseTimeParser extends IncrementalDatatypeParser {
  protected interface ParseStrategy {
    Object parse(String text, DateTimeFormatter formatter) throws DateTimeParseException;
  }

  protected final DateTimeFormatter[] formatters;
  protected final boolean[] replaceSpaces;
  protected final ParseStrategy parseStrategy;

  protected BaseTimeParser(String[] formats, Locale locale, ParseStrategy parseStrategy) {
    this.parseStrategy = parseStrategy;

    formatters = new DateTimeFormatter[formats.length];
    replaceSpaces = new boolean[formats.length];
    for (int i = 0; i < formats.length; i++) {
      formatters[i] = Time_Utils.make_formatter(formats[i], locale);
      replaceSpaces[i] = formats[i].endsWith("_DATE_TIME");
    }
  }

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    for (int i = 0; i < formatters.length; i++) {
      try {
        var replaced =
            replaceSpaces[i] && text.length() > 10 && text.charAt(10) == ' '
                ? new StringBuilder(text).replace(10, 11, "T").toString()
                : text;
        return parseStrategy.parse(replaced, formatters[i]);
      } catch (DateTimeParseException ignored) {
      }
    }

    problemAggregator.reportInvalidFormat(text);
    return null;
  }
}
