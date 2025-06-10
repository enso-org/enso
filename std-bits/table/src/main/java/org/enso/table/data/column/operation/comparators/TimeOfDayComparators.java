package org.enso.table.data.column.operation.comparators;

import java.time.LocalTime;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.storage.type.TimeOfDayType;

final class TimeOfDayComparators {
  public static final BinaryOperation<Boolean> EQ =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::equals, false);
  public static final BinaryOperation<Boolean> NEQ =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.equals(b), true);
  public static final BinaryOperation<Boolean> LT =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::isBefore);
  public static final BinaryOperation<Boolean> LTE =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.isAfter(b));
  public static final BinaryOperation<Boolean> GT =
      new GenericComparators<>(TimeOfDayType.INSTANCE, LocalTime::isAfter);
  public static final BinaryOperation<Boolean> GTE =
      new GenericComparators<>(TimeOfDayType.INSTANCE, (a, b) -> !a.isBefore(b));
}
