package org.enso.table.parsing;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.ObjectBuilder;
import org.enso.table.parsing.problems.InvalidFormatProblemAggregator;

public class TimeParser extends TypeParser<InvalidFormatProblemAggregator> {

  private final DateTimeFormatter[] formatters;

  public TimeParser(String[] formats, Locale locale) {
    formatters = new DateTimeFormatter[formats.length];
    for (int i = 0; i < formats.length; i++) {
      formatters[i] = DateTimeFormatter.ofPattern(formats[i], locale);
    }
  }

  @Override
  public Object parseSingleValue(String text, InvalidFormatProblemAggregator problemAggregator) {
    for (var formatter : formatters) {
      try {
        return LocalTime.parse(text, formatter);
      } catch (DateTimeParseException ignored) {
      }
    }
    
    problemAggregator.reportInvalidFormat(text);
    return null;
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    // Once datetime gets first-class support in our dataframes, a more specific builder type should
    // be used here.
    return new ObjectBuilder((int) capacity);
  }

  @Override
  public InvalidFormatProblemAggregator makeProblemAggregator() {
    return new InvalidFormatProblemAggregator();
  }
}
