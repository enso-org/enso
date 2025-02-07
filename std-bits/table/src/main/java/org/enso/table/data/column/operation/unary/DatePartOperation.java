package org.enso.table.data.column.operation.unary;

import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalField;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;

public class DatePartOperation implements UnaryOperation {
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

  public static final String DAY_OF_YEAR = "day_of_year";
  public static final UnaryOperation DAY_OF_YEAR_INSTANCE =
      new DatePartOperation(DAY_OF_YEAR, ChronoField.DAY_OF_YEAR, false);

  public static final String DAY_OF_WEEK = "day_of_week";
  public static final UnaryOperation DAY_OF_WEEK_INSTANCE =
      new DatePartOperation(DAY_OF_WEEK, ChronoField.DAY_OF_WEEK, false);

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

  protected final String name;
  protected final TemporalField field;
  protected final boolean timeField;

  protected DatePartOperation(String name, TemporalField field, boolean timeField) {
    this.name = name;
    this.field = field;
    this.timeField = timeField;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return timeField ? storage.getType().hasTime() : storage.getType().hasDate();
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> builder.appendLong(applyObjectRow(index, value)));
  }

  protected long applyObjectRow(long index, Object value) {
    if (value instanceof Temporal s) {
      return s.getLong(field);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected date/time type).");
    }
  }
}
