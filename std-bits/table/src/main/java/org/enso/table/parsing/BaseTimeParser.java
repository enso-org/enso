package org.enso.table.parsing;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.ObjectBuilder;
import org.enso.table.parsing.problems.ProblemAggregator;

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

  @Override
  protected Builder makeBuilderWithCapacity(long capacity) {
    // Once datetime gets first-class support in our dataframes, a more specific builder type should
    // be used.
    return new ObjectBuilder((int) capacity);
  }
}
