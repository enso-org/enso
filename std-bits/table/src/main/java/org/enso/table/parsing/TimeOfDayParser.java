package org.enso.table.parsing;

import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.TimeOfDayBuilder;
import org.enso.table.problems.ProblemAggregator;

public class TimeOfDayParser extends BaseTimeParser {
  public TimeOfDayParser(EnsoDateTimeFormatter[] formatters) {
    super(
        formatters,
        (String text, EnsoDateTimeFormatter formatter) -> formatter.parseLocalTime(text));
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity, ProblemAggregator problemAggregator) {
    return new TimeOfDayBuilder(capacity);
  }
}
