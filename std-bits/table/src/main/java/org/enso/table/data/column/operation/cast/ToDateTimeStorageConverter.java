package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.graalvm.polyglot.Context;

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
    Context context = Context.getCurrent();
    var builder = Builder.getForType(DateTimeType.INSTANCE, mixedStorage.size(), problemAggregator);
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case ZonedDateTime d -> builder.append(d);
        case LocalDate d -> builder.append(convertDate(d));
        default -> {
          problemAggregator.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }

      context.safepoint();
    }

    return (Storage<ZonedDateTime>) builder.seal();
  }

  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }

  private Storage<ZonedDateTime> convertDateStorage(
      DateStorage dateStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    var builder = Builder.getForType(DateTimeType.INSTANCE, dateStorage.size(), problemAggregator);
    for (int i = 0; i < dateStorage.size(); i++) {
      LocalDate date = dateStorage.getItem(i);
      builder.append(convertDate(date));
      context.safepoint();
    }

    return (Storage<ZonedDateTime>) builder.seal();
  }
}
