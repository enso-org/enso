package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForBoolean;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

/**
 * Binary operation for (Boolean, Boolean) => Boolean.
 * Override the `applySingle` method to perform a single computation.
 * For a specialized approach on top of BoolStorage, override either
 * the `applySpecializedMapOverBoolStorage` or the
 * `applySpecializedZipOverBoolStorage`. */
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
  public final ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
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
      var result = applySpecializedMapOverBoolStorage(leftBoolStorage, rightBoolean, rightIsNothing);
      if (result != null) {
        return result;
      }
    }

    return StorageIterators.buildOverBooleanStorage(
        BooleanType.INSTANCE.asTypedStorage(left),
        preserveNulls,
        makeStorageBuilder(left.getSize(), left.getType(), BooleanType.INSTANCE, problemAggregator),
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
  public final ColumnStorage<Boolean> applyZip(ColumnStorage<?> left, ColumnStorage<?> right, MapOperationProblemAggregator problemAggregator) {
    assert canApplyZip(left, right);

    if (right.getType() instanceof NullType) {
      return applyMap(left, null, problemAggregator);
    }

    if ((left instanceof BoolStorage leftBoolStorage)
        && (right instanceof BoolStorage rightBoolStorage)) {
      var result = applySpecializedZipOverBoolStorage(leftBoolStorage, rightBoolStorage);
      if (result != null) {
        return result;
      }
    }

    return StorageIterators.zipOverBooleanStorages(
        BooleanType.INSTANCE.asTypedStorage(left),
        BooleanType.INSTANCE.asTypedStorage(right),
        s -> makeStorageBuilder(s, left.getType(), right.getType(), problemAggregator),
        preserveNulls,
        (index, value, isNothing, rightValue, rightIsNothing) ->
            applySingle(value, isNothing, rightValue, rightIsNothing));
  }

  @Override
  protected BuilderForBoolean makeStorageBuilder(
      long size, StorageType<?> leftType, StorageType<?> rightType, ProblemAggregator problemAggregator) {
    return BooleanType.INSTANCE.makeBuilder(size, problemAggregator);
  }

  /**
   * Provides a specialized implementation for the map operation over BoolStorage.
   * @return Computed result or null to fallback to the standard implementation.
   */
  protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
      BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
    return null;
  }

  /**
   * Provides a specialized implementation for the zip operation over two BoolStorage objects.
   * @return Computed result or null to fallback to the standard implementation.
   */
  protected ColumnBooleanStorage applySpecializedZipOverBoolStorage(BoolStorage left, BoolStorage right) {
    return null;
  }

  protected abstract Boolean applySingle(
      boolean left, boolean isNothing, boolean right, boolean isNothingRight);
}
