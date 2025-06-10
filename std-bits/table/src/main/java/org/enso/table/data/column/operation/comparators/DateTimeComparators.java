package org.enso.table.data.column.operation.comparators;

import java.time.ZonedDateTime;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.storage.type.DateTimeType;

final class DateTimeComparators {
  public static final BinaryOperation<Boolean> EQ =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isEqual, false);
  public static final BinaryOperation<Boolean> NEQ =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.equals(b), true);
  public static final BinaryOperation<Boolean> LT =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isBefore);
  public static final BinaryOperation<Boolean> LTE =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.isAfter(b));
  public static final BinaryOperation<Boolean> GT =
      new GenericComparators<>(DateTimeType.INSTANCE, ZonedDateTime::isAfter);
  public static final BinaryOperation<Boolean> GTE =
      new GenericComparators<>(DateTimeType.INSTANCE, (a, b) -> !a.isBefore(b));
}
