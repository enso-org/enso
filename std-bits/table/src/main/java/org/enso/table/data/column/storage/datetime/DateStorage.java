package org.enso.table.data.column.storage.datetime;

import java.time.LocalDate;
import java.time.Period;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.DateBuilder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.numeric.UnaryIntegerOp;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;

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
    t.add(
        new GenericBinaryObjectMapOperation<LocalDate, SpecializedStorage<LocalDate>, Period>(Maps.SUB, LocalDate.class, DateStorage.class) {
          @Override
          protected Builder createOutputBuilder(int size) {
            return new DateBuilder(size);
          }

          @Override
          protected Period run(LocalDate value, LocalDate other) {
            return Period.between(other, value);
          }
        }
    );
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
