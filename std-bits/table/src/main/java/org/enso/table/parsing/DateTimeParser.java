package org.enso.table.parsing;

import java.time.DateTimeException;
import java.time.ZoneId;
import org.enso.base.time.EnsoDateTimeFormatter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.problems.ProblemAggregator;

public class DateTimeParser extends BaseTimeParser {
  public DateTimeParser(EnsoDateTimeFormatter[] formatters) {
    super(
        formatters,
        (String text, EnsoDateTimeFormatter formatter) -> {
          try {
            return formatter.parseZonedDateTime(text);
          } catch (DateTimeException e) {
            var localDate = formatter.parseLocalDate(text);
            return localDate.atStartOfDay(ZoneId.systemDefault());
          }
        });
  }

  @Override
  protected Builder makeBuilderWithCapacity(long capacity, ProblemAggregator problemAggregator) {
    return Builder.getForDateTime(capacity);
  }
}
