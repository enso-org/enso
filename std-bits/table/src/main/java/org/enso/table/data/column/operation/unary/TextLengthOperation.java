package org.enso.table.data.column.operation.unary;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.TextType;

public class TextLengthOperation implements UnaryOperation {
  public static final String NAME = "text_length";
  public static final UnaryOperation INSTANCE = new TextLengthOperation();

  private TextLengthOperation() {}

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
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> builder.appendLong(applyObjectRow(index, value)));
  }

  protected long applyObjectRow(long index, Object value) {
    if (value instanceof String s) {
      return Text_Utils.grapheme_length(s);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected text type).");
    }
  }
}
