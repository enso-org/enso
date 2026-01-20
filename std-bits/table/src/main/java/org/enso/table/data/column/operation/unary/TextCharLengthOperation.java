package org.enso.table.data.column.operation.unary;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class TextCharLengthOperation implements UnaryOperation {
  public static final String NAME = "text_char_length";
  public static final UnaryOperation INSTANCE = new TextCharLengthOperation();

  private TextCharLengthOperation() {}

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getType() instanceof TextType textType && textType.fixedLength()) {
      // Create a constant.
      return StorageIterators.buildOverStorage(
          storage,
          Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator),
          (builder, index, value) -> builder.appendLong(textType.maxLength()));
    }

    return StorageIterators.buildOverStorage(
        TextType.VARIABLE_LENGTH.asTypedStorage(storage),
        Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> builder.appendLong(applyObjectRow(index, value)));
  }

  protected long applyObjectRow(long index, Object value) {
    if (value instanceof String s) {
      return Text_Utils.char_length(s);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected text type).");
    }
  }
}
