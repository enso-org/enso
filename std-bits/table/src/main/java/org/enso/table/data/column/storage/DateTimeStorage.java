package org.enso.table.data.column.storage;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateBuilder;
import org.enso.table.data.column.builder.object.DateTimeBuilder;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.builder.object.TimeOfDayBuilder;
import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.UnaryIntegerOp;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.formatting.DateTimeFormatter;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateTimeStorage(ZonedDateTime[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> ops =
      buildOps();

  private static MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> buildOps() {
    MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(ZonedDateTime.class));
    t.add(
        new UnaryIntegerOp<>(Maps.YEAR) {
          @Override
          protected long doOperation(ZonedDateTime date) {
            return (long) date.getYear();
          }
        });
    t.add(
        new UnaryIntegerOp<>(Maps.MONTH) {
          @Override
          protected long doOperation(ZonedDateTime date) {
            return (long) date.getMonthValue();
          }
        });
    t.add(
        new UnaryIntegerOp<>(Maps.DAY) {
          @Override
          protected long doOperation(ZonedDateTime date) {
            return (long) date.getDayOfMonth();
          }
        });
    return t;
  }

  @Override
  protected SpecializedStorage<ZonedDateTime> newInstance(ZonedDateTime[] data, int size) {
    return new DateTimeStorage(data, size);
  }

  @Override
  protected ZonedDateTime[] newUnderlyingArray(int size) {
    return new ZonedDateTime[size];
  }

  @Override
  public StorageType getType() {
    return DateTimeType.INSTANCE;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new DateTimeBuilder(capacity);
  }

  @Override
  public Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    if (targetType instanceof DateType) {
      int n = size();
      DateBuilder builder = new DateBuilder(n);
      for (int i = 0; i < n; i++) {
        ZonedDateTime dateTime = data[i];
        if (dateTime == null) {
          builder.appendNulls(1);
        } else {
          LocalDate converted = dateTime.toLocalDate();
          builder.append(converted);
        }
      }
      return builder.seal();
    } else if (targetType instanceof TimeOfDayType) {
      int n = size();
      TimeOfDayBuilder builder = new TimeOfDayBuilder(n);
      for (int i = 0; i < n; i++) {
        ZonedDateTime dateTime = data[i];
        if (dateTime == null) {
          builder.appendNulls(1);
        } else {
          LocalTime converted = dateTime.toLocalTime();
          builder.append(converted);
        }
      }
      return builder.seal();
    } else if (targetType instanceof TextType textType) {
      int n = size();
      StringBuilder builder = new StringBuilder(n);
      var formatter = Core_Date_Utils.defaultZonedDateTimeFormatter();
      for (int i = 0; i < n; i++) {
        ZonedDateTime item = data[i];
        if (item == null) {
          builder.appendNulls(1);
        } else {
          builder.append(item.format(formatter));
        }
      }
      return StringStorage.adapt(builder.seal(), textType);
    } else {
      return super.cast(targetType, castProblemBuilder);
    }
  }
}
