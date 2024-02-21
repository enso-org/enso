package org.enso.table.data.column.operation.unary;

import java.time.temporal.ChronoField;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalField;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;

public class TruncatedTimePartOperation extends DatePartOperation {
  public static final String MICROSECOND = "microsecond";
  public static final UnaryOperation MICROSECOND_INSTANCE =
      new TruncatedTimePartOperation(MICROSECOND, ChronoField.MICRO_OF_SECOND, 1000);

  public static final String NANOSECOND = "nanosecond";
  public static final UnaryOperation NANOSECOND_INSTANCE =
      new TruncatedTimePartOperation(NANOSECOND, ChronoField.NANO_OF_SECOND, 1000);

  private final int truncation;

  private TruncatedTimePartOperation(String name, TemporalField field, int truncation) {
    super(name, field, true);
    this.truncation = truncation;
  }

  @Override
  protected void applyObjectRow(
      Object value, LongBuilder builder, MapOperationProblemAggregator problemAggregator) {
    if (value instanceof Temporal s) {
      var longValue = s.getLong(field);
      builder.appendLong(longValue % truncation);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected date/time type).");
    }
  }
}
