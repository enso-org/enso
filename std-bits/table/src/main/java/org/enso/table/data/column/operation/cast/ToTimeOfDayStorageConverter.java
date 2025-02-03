package org.enso.table.data.column.operation.cast;

import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

public class ToTimeOfDayStorageConverter implements StorageConverter<LocalTime> {
  @Override
  public Storage<LocalTime> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return timeOfDayStorage;
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType
        || storage.getType() instanceof NullType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Time_Of_Day.");
    }
  }

  private Storage<LocalTime> castFromMixed(
      ColumnStorage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForTime(mixedStorage.getSize()),
        mixedStorage,
        (i) -> {
          Object o = mixedStorage.getItemBoxed(i);
          return switch (o) {
            case LocalTime d -> d;
            case ZonedDateTime d -> convertDateTime(d);
            default -> {
              problemAggregator.reportConversionFailure(o);
              yield null;
            }
          };
        });
  }

  private Storage<LocalTime> convertDateTimeStorage(
      Storage<ZonedDateTime> dateTimeStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForTime(dateTimeStorage.getSize()),
        dateTimeStorage,
        (i) -> {
          ZonedDateTime dateTime = dateTimeStorage.getItemBoxed(i);
          return convertDateTime(dateTime);
        });
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }
}
