package org.enso.table.data.column.storage.datetime;

import java.time.LocalDate;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.bool.GenericBinaryOpReturningBoolean;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.UnexpectedTypeException;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, buildOps());
  }

  private static MapOperationStorage<LocalDate, SpecializedStorage<LocalDate>> buildOps() {
    MapOperationStorage<LocalDate, SpecializedStorage<LocalDate>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(LocalDate.class));
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.EQ, LocalDate.class) {
          @Override
          protected boolean doOperation(LocalDate a, LocalDate b) {
            return a.isEqual(b);
          }

          @Override
          protected boolean doOther(LocalDate a, Object b) {
            return false;
          }
        });
    t.add(
        new DateBinaryOpReturningBoolean(Maps.LT) {
          @Override
          protected boolean doOperation(LocalDate a, LocalDate b) {
            return a.compareTo(b) < 0;
          }
        });
    t.add(
        new DateBinaryOpReturningBoolean(Maps.LTE) {
          @Override
          protected boolean doOperation(LocalDate a, LocalDate b) {
            return a.compareTo(b) <= 0;
          }
        });
    t.add(
        new DateBinaryOpReturningBoolean(Maps.GT) {
          @Override
          protected boolean doOperation(LocalDate a, LocalDate b) {
            return a.compareTo(b) > 0;
          }
        });
    t.add(
        new DateBinaryOpReturningBoolean(Maps.GTE) {
          @Override
          protected boolean doOperation(LocalDate a, LocalDate b) {
            return a.compareTo(b) >= 0;
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

  private abstract static class DateBinaryOpReturningBoolean
      extends GenericBinaryOpReturningBoolean<LocalDate, SpecializedStorage<LocalDate>> {
    public DateBinaryOpReturningBoolean(String name) {
      super(name, LocalDate.class);
    }

    @Override
    protected boolean doOther(LocalDate a, Object b) {
      throw new UnexpectedTypeException("a Date", b.toString());
    }
  }
}
