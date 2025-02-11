package org.enso.table.data.column.operation.unary;

import java.time.LocalDate;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.DateTimeType;

public class DateTruncateOperation implements UnaryOperation {
  public static String TRUNCATE = "truncate";
  public static final UnaryOperation TRUNCATE_INSTANCE = new DateTruncateOperation();

  private DateTruncateOperation() {}

  @Override
  public String getName() {
    return TRUNCATE;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return storage.getType() == DateTimeType.INSTANCE;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage, Builder.getForDate(storage.getSize()), this::applyObjectRow);
  }

  protected LocalDate applyObjectRow(long index, Object value) {
    if (value instanceof ZonedDateTime zonedDateTime) {
      return zonedDateTime.toLocalDate();
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: Expected a Date_Time, got " + value.getClass());
    }
  }
}
