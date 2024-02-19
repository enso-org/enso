package org.enso.table.data.column.operation.unary;

import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalField;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;

public class DatePartOperation extends AbstractUnaryLongOperation {
  public static final String YEAR = "year";
  public static final UnaryOperation YEAR_INSTANCE =
      new DatePartOperation(YEAR, ChronoField.YEAR, false);

  public static final String QUARTER = "quarter";
  public static final UnaryOperation QUARTER_INSTANCE =
      new DatePartOperation(QUARTER, IsoFields.QUARTER_OF_YEAR, false);

  public static final String MONTH = "month";
  public static final UnaryOperation MONTH_INSTANCE =
      new DatePartOperation(MONTH, ChronoField.MONTH_OF_YEAR, false);

  public static final String WEEK = "week";
  public static final UnaryOperation WEEK_INSTANCE =
      new DatePartOperation(WEEK, IsoFields.WEEK_OF_WEEK_BASED_YEAR, false);

  public static final String DAY = "day";
  public static final UnaryOperation DAY_INSTANCE =
      new DatePartOperation(DAY, ChronoField.DAY_OF_MONTH, false);

  public static final String HOUR = "hour";
  public static final UnaryOperation HOUR_INSTANCE =
      new DatePartOperation(HOUR, ChronoField.HOUR_OF_DAY, true);

  public static final String MINUTE = "minute";
  public static final UnaryOperation MINUTE_INSTANCE =
      new DatePartOperation(MINUTE, ChronoField.MINUTE_OF_HOUR, true);

  public static final String SECOND = "second";
  public static final UnaryOperation SECOND_INSTANCE =
      new DatePartOperation(SECOND, ChronoField.SECOND_OF_MINUTE, true);

  public static final String MILLISECOND = "millisecond";
  public static final UnaryOperation MILLISECOND_INSTANCE =
      new DatePartOperation(MILLISECOND, ChronoField.MILLI_OF_SECOND, true);

  public static final String MICROSECOND = "microsecond";
  public static final UnaryOperation MICROSECOND_INSTANCE =
      new DatePartOperation(MICROSECOND, ChronoField.MICRO_OF_SECOND, true);

  public static final String NANOSECOND = "nanosecond";
  public static final UnaryOperation NANOSECOND_INSTANCE =
      new DatePartOperation(NANOSECOND, ChronoField.NANO_OF_SECOND, true);

  private final TemporalField field;
  private final boolean timeField;

  private DatePartOperation(String name, TemporalField field, boolean timeField) {
    super(name, true, IntegerType.INT_64);
    this.field = field;
    this.timeField = timeField;
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return timeField ? storage.getType().hasTime() : storage.getType().hasDate();
  }

  @Override
  protected void applyObjectRow(
      Object value, LongBuilder builder, MapOperationProblemAggregator problemAggregator) {
    if (value instanceof Temporal s) {
      var longValue = s.getLong(field);
      if (timeField) {
        longValue = longValue % 1000;
      }
      builder.appendLong(longValue);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected date/time type).");
    }
  }
}
