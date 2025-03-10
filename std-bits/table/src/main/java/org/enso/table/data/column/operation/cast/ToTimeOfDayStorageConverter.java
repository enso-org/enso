package org.enso.table.data.column.operation.cast;

import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToTimeOfDayStorageConverter implements StorageConverter<LocalTime> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType instanceof DateTimeType
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<LocalTime> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage);
    } else if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Time_Of_Day.");
    }
  }

  private ColumnStorage<LocalTime> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForTime(storage.getSize()),
        (index, value) ->
            switch (value) {
              case LocalTime d -> d;
              case ZonedDateTime d -> convertDateTime(d);
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }

  private ColumnStorage<LocalTime> convertDateTimeStorage(
      ColumnStorage<ZonedDateTime> dateTimeStorage) {
    return StorageIterators.mapOverStorage(
        dateTimeStorage,
        Builder.getForTime(dateTimeStorage.getSize()),
        (index, value) -> convertDateTime(value));
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }
}
