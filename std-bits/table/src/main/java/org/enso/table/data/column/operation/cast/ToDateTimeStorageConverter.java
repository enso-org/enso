package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.object.DateTimeBuilder;
import org.enso.table.data.column.storage.DateTimeStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class ToDateTimeStorageConverter implements StorageConverter<ZonedDateTime> {
  public ToDateTimeStorageConverter() {
  }

  public Storage<ZonedDateTime> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof DateTimeStorage dateTimeStorage) {
      return dateTimeStorage;
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  public Storage<ZonedDateTime> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    DateTimeBuilder builder = new DateTimeBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case ZonedDateTime d -> builder.append(d);
        case LocalDate d -> builder.append(convertDate(d));
        default -> {
          problemBuilder.reportConversionFailure();
          builder.appendNulls(1);
        }
      }
    }

    return builder.seal();
  }

  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }
}
