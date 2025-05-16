package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForBoolean;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.BlackholeProblemAggregator;

/** Binary operation for (Boolean, Boolean) => Boolean. */
public abstract class BinaryOperationBoolean extends BinaryOperationBase<Boolean> {
  private final boolean preserveNulls;

  public BinaryOperationBoolean() {
    this(true);
  }

  public BinaryOperationBoolean(boolean preserveNulls) {
    super(BooleanType.INSTANCE);
    this.preserveNulls = preserveNulls;
  }

  @Override
  public ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue) {
    assert canApplyMap(left, rightValue);

    if (preserveNulls && rightValue == null) {
      // Return an all null column
      return BoolStorage.makeEmpty(left.getSize());
    }

    if (rightValue != null && !(rightValue instanceof Boolean)) {
      throw new IllegalArgumentException(
          "Unsupported right value type " + rightValue.getClass() + ".");
    }

    boolean rightIsNothing = rightValue == null;
    boolean rightBoolean = !rightIsNothing && (boolean) rightValue;

    if (left instanceof BoolStorage leftBoolStorage) {
      var result = applyMapOverBoolStorage(leftBoolStorage, rightBoolean, rightIsNothing);
      if (result != null) {
        return result;
      }
    }

    return StorageIterators.buildOverBooleanStorage(
        BooleanType.INSTANCE.asTypedStorage(left),
        preserveNulls,
        makeStorageBuilder(left.getSize(), left.getType(), BooleanType.INSTANCE),
        (b, index, value, isNothing) -> {
          Boolean result = applySingle(value, isNothing, rightBoolean, rightIsNothing);
          if (result == null) {
            b.appendNulls(1);
          } else {
            b.appendBoolean(result);
          }
        });
  }

  @Override
  public ColumnStorage<Boolean> applyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    assert canApplyZip(left, right);

    if (right.getType() instanceof NullType) {
      return applyMap(left, null);
    }

    if ((left instanceof BoolStorage leftBoolStorage)
        && (right instanceof BoolStorage rightBoolStorage)) {
      var result = applyZipOverBoolStorage(leftBoolStorage, rightBoolStorage);
      if (result != null) {
        return result;
      }
    }

    return StorageIterators.zipOverBooleanStorages(
        BooleanType.INSTANCE.asTypedStorage(left),
        BooleanType.INSTANCE.asTypedStorage(right),
        s -> makeStorageBuilder(s, left.getType(), right.getType()),
        preserveNulls,
        (index, value, isNothing, rightValue, rightIsNothing) ->
            applySingle(value, isNothing, rightValue, rightIsNothing));
  }

  @Override
  protected BuilderForBoolean makeStorageBuilder(
      long size, StorageType<?> leftType, StorageType<?> rightType) {
    return BooleanType.INSTANCE.makeBuilder(size, BlackholeProblemAggregator.INSTANCE);
  }

  protected ColumnBooleanStorage applyMapOverBoolStorage(
      BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
    return null;
  }

  protected ColumnBooleanStorage applyZipOverBoolStorage(BoolStorage left, BoolStorage right) {
    return null;
  }

  protected abstract Boolean applySingle(
      boolean left, boolean isNothing, boolean right, boolean isNothingRight);
}
