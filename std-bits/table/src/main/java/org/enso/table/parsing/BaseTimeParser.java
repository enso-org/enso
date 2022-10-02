package org.enso.table.parsing;

import org.enso.table.parsing.problems.ProblemAggregator;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;

public abstract class BaseTimeParser extends IncrementalDatatypeParser {
  protected interface ParseStrategy {
    Object parse(String text, DateTimeFormatter formatter) throws DateTimeParseException;
  }

  protected final DateTimeFormatter[] formatters;
  protected final ParseStrategy parseStrategy;

  protected BaseTimeParser(String[] formats, Locale locale, ParseStrategy parseStrategy) {
    this.parseStrategy = parseStrategy;

    formatters = new DateTimeFormatter[formats.length];
    for (int i = 0; i < formats.length; i++) {
      formatters[i] = DateTimeFormatter.ofPattern(formats[i], locale);
    }
  }

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    for (var formatter : formatters) {
      try {
        return parseStrategy.parse(text, formatter);
      } catch (DateTimeParseException ignored) {
      }
    }

    problemAggregator.reportInvalidFormat(text);
    return null;
  }
}
