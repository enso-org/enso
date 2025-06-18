package org.enso.table.data.column.operation.comparators;

import java.time.ZonedDateTime;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.type.DateTimeType;

final class DateTimeComparators {
  public static final BinaryOperationTyped<Boolean> EQ =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isEqual, false);
  public static final BinaryOperationTyped<Boolean> NEQ =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.equals(b), true);
  public static final BinaryOperationTyped<Boolean> LT =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isBefore);
  public static final BinaryOperationTyped<Boolean> LTE =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.isAfter(b));
  public static final BinaryOperationTyped<Boolean> GT =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isAfter);
  public static final BinaryOperationTyped<Boolean> GTE =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.isBefore(b));
}
