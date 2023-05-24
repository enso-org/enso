package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.object.TimeOfDayBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

import java.time.LocalTime;
import java.time.ZonedDateTime;

public class ToTimeOfDayStorageConverter implements StorageConverter<LocalTime> {
  public ToTimeOfDayStorageConverter() {
  }

  public Storage<LocalTime> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return timeOfDayStorage;
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
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
          problemBuilder.reportConversionFailure();
          builder.appendNulls(1);
        }
      }
    }

    return builder.seal();
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }
}
