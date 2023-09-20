package org.enso.table.parsing;

import java.time.format.DateTimeParseException;
import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.parsing.problems.ProblemAggregator;

public abstract class BaseTimeParser extends IncrementalDatatypeParser {
  protected interface ParseStrategy {
    Object parse(String text, EnsoDateTimeFormatter formatter) throws DateTimeParseException;
  }

  protected final EnsoDateTimeFormatter[] formatters;
  protected final ParseStrategy parseStrategy;

  protected BaseTimeParser(EnsoDateTimeFormatter[] formatters, ParseStrategy parseStrategy) {
    this.parseStrategy = parseStrategy;
    this.formatters = formatters;
  }

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    for (EnsoDateTimeFormatter formatter : formatters) {
      try {
        return parseStrategy.parse(text, formatter);
      } catch (DateTimeParseException ignored) {
        // TODO I think ideally we should try to return Option instead of throwing, as throwing is
        // inefficient
      }
    }

    problemAggregator.reportInvalidFormat(text);
    return null;
  }
}
