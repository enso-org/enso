package org.enso.table.data.column.operation.binary;

import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.text.TextConcatenate;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;

/**
 * Support the addition operation
 * - Numeric
 * - Text Concatenation
 * - Date + Time => Date Time ??
 */
public class AddOperation {
  /**
   *
   * @param left
   * @param right
   * @return
   */
  public static BinaryOperation<?> createIfSupported(Column left, Column right) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case TextType tt -> TextConcatenate.INSTANCE;
      default -> null;
    };
  }
}
