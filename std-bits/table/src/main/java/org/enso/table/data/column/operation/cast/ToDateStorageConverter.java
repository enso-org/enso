package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateType;
import org.graalvm.polyglot.Context;

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

  public Storage<LocalDate> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    var builder = Builder.getForType(DateType.INSTANCE, mixedStorage.size(), problemAggregator);
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case LocalDate d -> builder.append(d);
        case ZonedDateTime d -> builder.append(convertDateTime(d));
        default -> {
          problemAggregator.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }

      context.safepoint();
    }

    return (Storage<LocalDate>) builder.seal();
  }

  private LocalDate convertDateTime(ZonedDateTime dateTime) {
    return dateTime.toLocalDate();
  }

  private Storage<LocalDate> convertDateTimeStorage(
      DateTimeStorage dateTimeStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    var builder = Builder.getForType(DateType.INSTANCE, dateTimeStorage.size(), problemAggregator);
    for (int i = 0; i < dateTimeStorage.size(); i++) {
      ZonedDateTime dateTime = dateTimeStorage.getItem(i);
      builder.append(convertDateTime(dateTime));
      context.safepoint();
    }

    return (Storage<LocalDate>) builder.seal();
  }
}
