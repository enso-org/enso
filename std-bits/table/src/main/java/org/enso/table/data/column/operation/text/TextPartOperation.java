package org.enso.table.data.column.operation.text;

import org.enso.base.Text_Utils;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBase;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public final class TextPartOperation extends BinaryOperationBase<String, String> {
  public static final TextPartOperation LEFT = new TextPartOperation(Text_Utils::take_prefix);
  public static final TextPartOperation RIGHT = new TextPartOperation(Text_Utils::take_suffix);

  @FunctionalInterface
  public interface TextLongToStringFunction {
    String apply(String text, long value);
  }

  private final TextLongToStringFunction function;

  private TextPartOperation(TextLongToStringFunction function) {
    super(TextType.VARIABLE_LENGTH, TextType.VARIABLE_LENGTH, true);
    this.function = function;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    var rightStorageType = right.getType();
    return canApplyMap(left, null)
        && (rightStorageType instanceof IntegerType || rightStorageType instanceof NullType);
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

    if (!NumericConverter.isCoercibleToLong(rightValue)) {
      throw new IllegalArgumentException("Unsupported right value type.");
    }
    long right = NumericConverter.coerceToLong(rightValue);

    return StorageIterators.mapOverStorage(
        left,
        Builder.getForText(textType, left.getSize()),
        (index, value) -> function.apply(value, right));
  }

  @Override
  protected ColumnStorage<String> applyTypedZip(
      ColumnStorage<String> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (!(left.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException("Left type is not a text type");
    }

    if (right.getType() instanceof IntegerType integerType) {
      return StorageIterators.zipOverStorages(
          left,
          integerType.asTypedStorage(right),
          length -> Builder.getForText(textType, length),
          true,
          (index, leftValue, rightValue) -> function.apply(leftValue, rightValue));
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }
}
