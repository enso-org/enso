package org.enso.table.data.column.operation.text;

import org.enso.base.Text_Utils;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;

public final class TextPartOperation implements BinaryOperation<String> {
  public static final TextPartOperation LEFT = new TextPartOperation(Text_Utils::take_prefix);
  public static final TextPartOperation RIGHT = new TextPartOperation(Text_Utils::take_suffix);

  @FunctionalInterface
  public interface TextLongToStringFunction {
    String apply(String text, long value);
  }

  private final TextLongToStringFunction function;

  private TextPartOperation(TextLongToStringFunction function) {
    this.function = function;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var storageType = left.getType();
    return storageType instanceof TextType || storageType instanceof NullType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    var rightStorageType = right.getType();
    return canApplyMap(left, null)
        && (rightStorageType instanceof IntegerType || rightStorageType instanceof NullType);
  }

  @Override
  public ColumnStorage<String> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType) {
      return StringBuilder.makeEmpty(TextType.VARIABLE_LENGTH, left.getSize());
    }

    if (left.getType() instanceof TextType textType) {
      if (rightValue == null) {
        return StringBuilder.makeEmpty(textType, left.getSize());
      }

      if (!NumericConverter.isCoercibleToLong(rightValue)) {
        throw new IllegalArgumentException("Unsupported right value type.");
      }
      long right = NumericConverter.coerceToLong(rightValue);

      return StorageIterators.mapOverStorage(
          textType.asTypedStorage(left),
          Builder.getForText(textType, left.getSize()),
          (index, value) -> function.apply(value, right));
    }

    throw new IllegalArgumentException("Unsupported storage type.");
  }

  @Override
  public ColumnStorage<String> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getSize() != right.getSize()) {
      throw new IllegalArgumentException("Columns must be of the same size.");
    }

    if (left.getType() instanceof NullType || right.getType() instanceof NullType) {
      return StringBuilder.makeEmpty(TextType.VARIABLE_LENGTH, left.getSize());
    }

    if (left.getType() instanceof TextType textType
        && right.getType() instanceof IntegerType integerType) {
      return StorageIterators.zipOverStorages(
          textType.asTypedStorage(left),
          integerType.asTypedStorage(right),
          length -> Builder.getForText(textType, length),
          true,
          (index, leftValue, rightValue) -> function.apply(leftValue, rightValue));
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }
}
