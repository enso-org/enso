package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class ToDateStorageConverter implements StorageConverter<LocalDate> {
  @Override
  public Storage<LocalDate> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DateStorage dateStorage) {
      return dateStorage;
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Date.");
    }
  }

  private Storage<LocalDate> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForDate(mixedStorage.size()),
        mixedStorage,
        (i) -> {
          Object o = mixedStorage.getItemBoxed(i);
          return switch (o) {
            case LocalDate d -> d;
            case ZonedDateTime d -> d.toLocalDate();
            default -> {
              problemAggregator.reportConversionFailure(o);
              yield null;
            }
          };
        });
  }

  private Storage<LocalDate> convertDateTimeStorage(
      DateTimeStorage dateTimeStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForDate(dateTimeStorage.size()),
        dateTimeStorage,
        (i) -> {
          ZonedDateTime dateTime = dateTimeStorage.getItem(i);
          return dateTime.toLocalDate();
        });
  }
}
