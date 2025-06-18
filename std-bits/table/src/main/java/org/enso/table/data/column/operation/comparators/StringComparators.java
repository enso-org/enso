package org.enso.table.data.column.operation.comparators;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.type.TextType;

final class StringComparators {
  public static final BinaryOperationTyped<Boolean> EQ =
      new GenericComparators<>(TextType.VARIABLE_LENGTH, Text_Utils::equals, false);
  public static final BinaryOperationTyped<Boolean> NEQ =
      new GenericComparators<>(TextType.VARIABLE_LENGTH, (a, b) -> !Text_Utils.equals(a, b), false);
  public static final BinaryOperationTyped<Boolean> LT =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) < 0);
  public static final BinaryOperationTyped<Boolean> LTE =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) <= 0);
  public static final BinaryOperationTyped<Boolean> GT =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) > 0);
  public static final BinaryOperationTyped<Boolean> GTE =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) >= 0);
}
