package org.enso.table.data.column.operation.comparators;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.storage.type.TextType;

final class StringComparators {
  public static final BinaryOperation<Boolean> EQ =
      new GenericComparators<>(TextType.VARIABLE_LENGTH, Text_Utils::equals, false);
  public static final BinaryOperation<Boolean> NEQ =
      new GenericComparators<>(TextType.VARIABLE_LENGTH, (a, b) -> !Text_Utils.equals(a, b), false);
  public static final BinaryOperation<Boolean> LT =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) < 0);
  public static final BinaryOperation<Boolean> LTE =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) <= 0);
  public static final BinaryOperation<Boolean> GT =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) > 0);
  public static final BinaryOperation<Boolean> GTE =
      new GenericComparators<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) >= 0);
}
