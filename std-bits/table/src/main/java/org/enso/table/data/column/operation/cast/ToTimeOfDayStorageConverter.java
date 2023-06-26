package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.TimeOfDayBuilder;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

import java.time.LocalTime;
import java.time.ZonedDateTime;

public class ToTimeOfDayStorageConverter implements StorageConverter<LocalTime> {
  public Storage<LocalTime> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return timeOfDayStorage;
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return convertDateTimeStorage(dateTimeStorage);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Time_Of_Day.");
    }
  }

  public Storage<LocalTime> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    TimeOfDayBuilder builder = new TimeOfDayBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case LocalTime d -> builder.append(d);
        case ZonedDateTime d -> builder.append(convertDateTime(d));
        default -> {
          problemBuilder.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }
    }

    return builder.seal();
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }

  private Storage<LocalTime> convertDateTimeStorage(DateTimeStorage dateTimeStorage) {
    TimeOfDayBuilder builder = new TimeOfDayBuilder(dateTimeStorage.size());
    for (int i = 0; i < dateTimeStorage.size(); i++) {
      ZonedDateTime dateTime = dateTimeStorage.getItem(i);
      builder.append(convertDateTime(dateTime));
    }
    return builder.seal();
  }
}
