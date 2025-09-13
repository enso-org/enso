package org.enso.table.data.column.operation.comparators;

import java.time.LocalTime;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.type.TimeOfDayType;

final class TimeOfDayComparators {
  public static final BinaryOperationTyped<Boolean> EQ =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::equals, false);
  public static final BinaryOperationTyped<Boolean> NEQ =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.equals(b), true);
  public static final BinaryOperationTyped<Boolean> LT =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::isBefore);
  public static final BinaryOperationTyped<Boolean> LTE =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.isAfter(b));
  public static final BinaryOperationTyped<Boolean> GT =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::isAfter);
  public static final BinaryOperationTyped<Boolean> GTE =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.isBefore(b));
}
