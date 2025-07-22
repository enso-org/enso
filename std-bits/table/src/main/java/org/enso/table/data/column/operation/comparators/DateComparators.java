package org.enso.table.data.column.operation.comparators;

import java.time.LocalDate;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.type.DateType;

final class DateComparators {
  public static final BinaryOperationTyped<Boolean> EQ =
      new GenericComparators<>(DateType.INSTANCE, LocalDate::isEqual, false);
  public static final BinaryOperationTyped<Boolean> NEQ =
      new GenericComparators<>(DateType.INSTANCE, (a, b) -> !a.isEqual(b), true);
  public static final BinaryOperationTyped<Boolean> LT =
      new GenericComparators<>(DateType.INSTANCE, LocalDate::isBefore);
  public static final BinaryOperationTyped<Boolean> LTE =
      new GenericComparators<>(DateType.INSTANCE, (a, b) -> !a.isAfter(b));
  public static final BinaryOperationTyped<Boolean> GT =
      new GenericComparators<>(DateType.INSTANCE, LocalDate::isAfter);
  public static final BinaryOperationTyped<Boolean> GTE =
      new GenericComparators<>(DateType.INSTANCE, (a, b) -> !a.isBefore(b));
}
