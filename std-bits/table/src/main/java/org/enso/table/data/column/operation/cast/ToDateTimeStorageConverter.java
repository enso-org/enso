package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class ToDateTimeStorageConverter implements StorageConverter<ZonedDateTime> {
  @Override
  public Storage<ZonedDateTime> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DateTimeStorage dateTimeStorage) {
      return dateTimeStorage;
    } else if (storage instanceof DateStorage dateStorage) {
      return convertDateStorage(dateStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Date_Time.");
    }
  }

  public Storage<ZonedDateTime> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForDateTime(mixedStorage.size()),
        mixedStorage,
        (i) -> {
          Object o = mixedStorage.getItemBoxed(i);
          return switch (o) {
            case ZonedDateTime d -> d;
            case LocalDate d -> convertDate(d);
            default -> {
              problemAggregator.reportConversionFailure(o);
              yield null;
            }
          };
        });
  }

  private Storage<ZonedDateTime> convertDateStorage(
      DateStorage dateStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForDateTime(dateStorage.size()),
        dateStorage,
        (i) -> {
          LocalDate date = dateStorage.getItem(i);
          return convertDate(date);
        });
  }

  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }
}
