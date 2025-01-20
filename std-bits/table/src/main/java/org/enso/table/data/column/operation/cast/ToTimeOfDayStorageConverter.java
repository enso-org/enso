package org.enso.table.data.column.operation.cast;

import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class ToTimeOfDayStorageConverter implements StorageConverter<LocalTime> {
  @Override
  public Storage<LocalTime> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return timeOfDayStorage;
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Time_Of_Day.");
    }
  }

  private Storage<LocalTime> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForTime(mixedStorage.size()),
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
      DateTimeStorage dateTimeStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForTime(dateTimeStorage.size()),
        dateTimeStorage,
        (i) -> {
          ZonedDateTime dateTime = dateTimeStorage.getItem(i);
          return convertDateTime(dateTime);
        });
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }
}
