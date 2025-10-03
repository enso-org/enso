package org.enso.table.data.column.operation.text;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBase;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;

public class TextConcatenate extends BinaryOperationBase<String, String> {
  public static final TextConcatenate INSTANCE = new TextConcatenate();

  private TextConcatenate() {
    super(TextType.VARIABLE_LENGTH, TextType.VARIABLE_LENGTH, true);
  }

  @Override
  protected ColumnStorage<String> applyNullMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    return Builder.makeEmpty(TextType.VARIABLE_LENGTH, left.getSize());
  }

  @Override
  protected ColumnStorage<String> applyTypedMap(
      ColumnStorage<String> left,
      Object rightValue,
      MapOperationProblemAggregator problemAggregator) {
    if (!(left.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (rightValue == null) {
      return Builder.makeEmpty(textType, left.getSize());
    }

    String typedRightValue = textType.valueAsType(rightValue);
    if (typedRightValue == null) {
      throw new UnexpectedTypeException("a Text");
    }

    // Compute the combined type
    TextType rightType = TextType.preciseTypeForValue(typedRightValue);
    TextType newType = TextType.concatTypes(textType, rightType);

    return StorageIterators.mapOverStorage(
        left,
        Builder.getForText(newType, left.getSize()),
        (index, value) -> value + typedRightValue);
  }

  @Override
  protected ColumnStorage<String> applyTypedZip(
      ColumnStorage<String> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (!(left.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (right.getType() instanceof TextType rightType) {
      TextType newType = TextType.concatTypes(textType, rightType);
      return StorageIterators.zipOverStorages(
          left,
          rightType.asTypedStorage(right),
          length -> Builder.getForText(newType, length),
          true,
          (index, leftValue, rightValue) -> leftValue + rightValue);
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }
}
