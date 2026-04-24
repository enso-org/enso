package org.enso.table.data.column.operation.text;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBase;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;

abstract class AbstractTextIndexOfOperation extends BinaryOperationBase<String, Long> {
  protected AbstractTextIndexOfOperation() {
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
    if (!(StorageType.ofStorage(left) instanceof TextType textType)) {
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
        IntegerType.INT_64.makeBuilder(left.getSize(), problemAggregator),
        (index, value) -> calculateIndex(value, typedRightValue));
  }

  @Override
  protected ColumnStorage<Long> applyTypedZip(
      ColumnStorage<String> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (!(StorageType.ofStorage(left) instanceof TextType _)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (StorageType.ofStorage(right) instanceof TextType rightType) {
      return StorageIterators.zipOverStorages(
          left,
          rightType.asTypedStorage(right),
          length -> IntegerType.INT_64.makeBuilder(length, problemAggregator),
          true,
          (index, leftValue, rightValue) -> calculateIndex(leftValue, rightValue));
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }

  protected abstract int findCodeunitIndex(String value, String needle);

  private long calculateIndex(String value, String needle) {
    int codeunitIndex = findCodeunitIndex(value, needle);
    if (codeunitIndex == -1) {
      return -1;
    }
    return Text_Utils.utf16_index_to_grapheme_index(value, codeunitIndex);
  }
}
