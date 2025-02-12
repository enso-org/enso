package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToDateStorageConverter implements StorageConverter<LocalDate> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType instanceof DateTimeType
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<LocalDate> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage);
    } else if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Date.");
    }
  }

  private ColumnStorage<LocalDate> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForDate(storage.getSize()),
        (index, value) ->
            switch (value) {
              case LocalDate d -> d;
              case ZonedDateTime d -> d.toLocalDate();
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }

  private ColumnStorage<LocalDate> convertDateTimeStorage(
      ColumnStorage<ZonedDateTime> dateTimeStorage) {
    return StorageIterators.mapOverStorage(
        dateTimeStorage,
        Builder.getForDate(dateTimeStorage.getSize()),
        (index, value) -> value.toLocalDate());
  }
}
