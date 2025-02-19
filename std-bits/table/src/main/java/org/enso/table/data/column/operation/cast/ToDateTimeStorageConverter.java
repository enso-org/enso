package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToDateTimeStorageConverter implements StorageConverter<ZonedDateTime> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType instanceof DateType
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<ZonedDateTime> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DateStorage dateStorage) {
      return convertDateStorage(dateStorage);
    } else if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Date_Time.");
    }
  }

  public ColumnStorage<ZonedDateTime> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForDateTime(storage.getSize()),
        (index, value) ->
            switch (value) {
              case ZonedDateTime d -> d;
              case LocalDate d -> convertDate(d);
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }

  private ColumnStorage<ZonedDateTime> convertDateStorage(ColumnStorage<LocalDate> dateStorage) {
    return StorageIterators.mapOverStorage(
        dateStorage,
        Builder.getForDateTime(dateStorage.getSize()),
        (index, value) -> convertDate(value));
  }

  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }
}
