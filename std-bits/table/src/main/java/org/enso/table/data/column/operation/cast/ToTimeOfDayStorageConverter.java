package org.enso.table.data.column.operation.cast;

import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.TimeOfDayBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.graalvm.polyglot.Context;

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

  public Storage<LocalTime> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    TimeOfDayBuilder builder = new TimeOfDayBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case LocalTime d -> builder.append(d);
        case ZonedDateTime d -> builder.append(convertDateTime(d));
        default -> {
          problemAggregator.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private LocalTime convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalTime();
  }

  private Storage<LocalTime> convertDateTimeStorage(
      DateTimeStorage dateTimeStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    TimeOfDayBuilder builder = new TimeOfDayBuilder(dateTimeStorage.size());
    for (int i = 0; i < dateTimeStorage.size(); i++) {
      ZonedDateTime dateTime = dateTimeStorage.getItem(i);
      builder.append(convertDateTime(dateTime));

      context.safepoint();
    }

    return builder.seal();
  }
}
