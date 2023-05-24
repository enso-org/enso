package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.UnaryIntegerOp;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;

import java.time.LocalDate;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> ops = buildOps();

  private static MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> buildOps() {
    MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> t = ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(LocalDate.class));
    t.add(
        new UnaryIntegerOp<>(Maps.YEAR) {
          @Override
          protected long doOperation(LocalDate date) {
            return (long) date.getYear();
          }
        });
    t.add(
        new UnaryIntegerOp<>(Maps.MONTH) {
          @Override
          protected long doOperation(LocalDate date) {
            return (long) date.getMonthValue();
          }
        });
    t.add(
        new UnaryIntegerOp<>(Maps.DAY) {
          @Override
          protected long doOperation(LocalDate date) {
            return (long) date.getDayOfMonth();
          }
        });
    return t;
  }

  @Override
  protected SpecializedStorage<LocalDate> newInstance(LocalDate[] data, int size) {
    return new DateStorage(data, size);
  }

  @Override
  protected LocalDate[] newUnderlyingArray(int size) {
    return new LocalDate[size];
  }

  @Override
  public StorageType getType() {
    return DateType.INSTANCE;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new DateBuilder(capacity);
  }
}
