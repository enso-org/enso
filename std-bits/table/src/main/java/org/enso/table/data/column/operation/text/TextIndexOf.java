package org.enso.table.data.column.operation.text;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBase;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;

public class TextIndexOf extends BinaryOperationBase<String, Long> {
  public static final TextIndexOf INSTANCE = new TextIndexOf();

  private TextIndexOf() {
    super(TextType.VARIABLE_LENGTH, IntegerType.INT_64, true);
  }

  @Override
  protected ColumnStorage<Long> applyNullMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    return Builder.makeEmpty(IntegerType.INT_64, left.getSize());
  }

  @Override
  protected ColumnStorage<Long> applyTypedMap(
      ColumnStorage<String> left,
      Object rightValue,
      MapOperationProblemAggregator problemAggregator) {
    if (!(left.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (rightValue == null) {
      return Builder.makeEmpty(IntegerType.INT_64, left.getSize());
    }

    String typedRightValue = textType.valueAsType(rightValue);
    if (typedRightValue == null) {
      throw new UnexpectedTypeException("a Text");
    }

    return StorageIterators.mapOverStorage(
        left,
        Builder.getForLong(IntegerType.INT_64, left.getSize(), problemAggregator),
        (index, value) -> calculateIndex(value, typedRightValue));
  }

  @Override
  protected ColumnStorage<Long> applyTypedZip(
      ColumnStorage<String> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (!(left.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (right.getType() instanceof TextType rightType) {
      return StorageIterators.zipOverStorages(
          left,
          rightType.asTypedStorage(right),
          length -> Builder.getForLong(IntegerType.INT_64, length, problemAggregator),
          true,
          (index, leftValue, rightValue) -> calculateIndex(leftValue, rightValue));
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }

  private static long calculateIndex(String value, String needle) {
    int codeunitIndex = value.indexOf(needle);
    if (codeunitIndex == -1) {
      return -1;
    }
    return Text_Utils.utf16_index_to_grapheme_index(value, codeunitIndex);
  }
}
