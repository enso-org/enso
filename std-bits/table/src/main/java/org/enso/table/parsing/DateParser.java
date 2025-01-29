package org.enso.table.parsing;

import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.problems.ProblemAggregator;

public class DateParser extends BaseTimeParser {
  public DateParser(EnsoDateTimeFormatter[] formatters) {
    super(
        formatters,
        (String text, EnsoDateTimeFormatter formatter) -> formatter.parseLocalDate(text));
  }

  @Override
  protected Builder makeBuilderWithCapacity(long capacity, ProblemAggregator problemAggregator) {
    return Builder.getForDate(capacity);
  }
}
