package org.enso.table.data.column.operation;

import org.enso.base.CompareException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForBoolean;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

/**
 * Binary operation for (Boolean, Boolean) => Boolean. Override the `applySingle` method to perform
 * a single computation. For a specialized approach on top of BoolStorage, override either the
 * `applySpecializedMapOverBoolStorage` or the `applySpecializedZipOverBoolStorage`. Likewise for
 * NullStorage, override the `applySpecializedMapOverNullStorage` or the
 * `applySpecializedZipOverNullStorage`.
 */
public abstract class BinaryOperationBoolean extends BinaryOperationBase<Boolean, Boolean> {
  private final boolean preserveNulls;
  protected final boolean throwOnOther;
  protected final boolean valueOnOther;

  public BinaryOperationBoolean() {
    this(true, false);
  }

  protected BinaryOperationBoolean(boolean preserveNulls, boolean allowNullType) {
    super(BooleanType.INSTANCE, BooleanType.INSTANCE, allowNullType);
    this.preserveNulls = preserveNulls;
    this.throwOnOther = true;
    this.valueOnOther = false;
  }

  protected BinaryOperationBoolean(
      boolean preserveNulls, boolean allowNullType, boolean valueOnOther) {
    super(BooleanType.INSTANCE, BooleanType.INSTANCE, allowNullType);
    this.preserveNulls = preserveNulls;
    this.throwOnOther = false;
    this.valueOnOther = valueOnOther;
  }

  protected boolean onIncomparable(Object left, Object right) {
    if (throwOnOther) {
      throw new CompareException(left, right);
    }
    return valueOnOther;
  }

  @Override
  public final ColumnStorage<Boolean> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    assert canApplyMap(left, rightValue);

    if (preserveNulls && rightValue == null) {
      // Return an all null column
      return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
    }

    if (rightValue != null && !(rightValue instanceof Boolean)) {
      // If all are Nothing then will return a Nothing Boolean Storage
      return StorageIterators.buildOverStorage(
          left,
          BooleanType.INSTANCE.makeBuilder(left.getSize(), problemAggregator),
          (b, index, value) -> b.appendBoolean(onIncomparable(value, rightValue)));
    }

    boolean rightIsNothing = rightValue == null;
    boolean rightBoolean = !rightIsNothing && (boolean) rightValue;

    if (left.getType() instanceof NullType) {
      return applySpecializedMapOverNullStorage(
          left, rightBoolean, rightIsNothing, problemAggregator);
    }

    if (left instanceof BoolStorage leftBoolStorage) {
      var result =
          applySpecializedMapOverBoolStorage(
              leftBoolStorage, rightBoolean, rightIsNothing, problemAggregator);
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
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    if (!canApplyMap(left, right)) {
      return false;
    }

    // If not throwing on other types, we can apply the operation
    // Otherwise, we allow Any, Boolean, and Null types on the right
    if (!throwOnOther) {
      return true;
    }

    var rightType = right.getType();
    return switch (rightType) {
      case NullType nt -> true;
      case BooleanType bt -> true;
      case AnyObjectType ay -> true;
      default -> false;
    };
  }

  @Override
  public final ColumnStorage<Boolean> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    assert canApplyZip(left, right);

    if (right.getType() instanceof NullType) {
      return applyMap(left, null, problemAggregator);
    }

    if (left.getType() instanceof NullType) {
      return applySpecializedZipOverNullStorage(left, right, problemAggregator);
    }

    if ((left instanceof BoolStorage leftBoolStorage)
        && (right instanceof BoolStorage rightBoolStorage)) {
      var result =
          applySpecializedZipOverBoolStorage(leftBoolStorage, rightBoolStorage, problemAggregator);
      if (result != null) {
        return result;
      }
    }

    if (!BooleanType.INSTANCE.isOfType(right.getType())) {
      // Have a mismatch in types (could be AnyObjectType)
      return StorageIterators.zipOverStorages(
          BooleanType.INSTANCE.asTypedStorage(left),
          right,
          Builder::getForBoolean,
          preserveNulls,
          (index, leftValue, rightValue) -> {
            boolean leftBoolean = leftValue != null && (boolean) leftValue;
            Boolean typedRightValue = BooleanType.INSTANCE.valueAsType(rightValue);
            boolean rightBoolean = typedRightValue != null && typedRightValue;
            return rightValue != null && typedRightValue == null
                ? onIncomparable(leftValue, rightValue)
                : applySingle(leftBoolean, leftValue == null, rightBoolean, rightValue == null);
          });
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
      long size,
      StorageType<?> leftType,
      StorageType<?> rightType,
      ProblemAggregator problemAggregator) {
    return BooleanType.INSTANCE.makeBuilder(size, problemAggregator);
  }

  /**
   * Provides a specialized implementation for the map operation over null storage.
   *
   * @return Computed result.
   */
  protected ColumnStorage<Boolean> applySpecializedMapOverNullStorage(
      ColumnStorage<?> left,
      boolean rightBoolean,
      boolean rightIsNothing,
      MapOperationProblemAggregator problemAggregator) {
    if (preserveNulls) {
      return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
    } else {
      throw new IllegalStateException(
          "Cannot apply map operation over null storage with preserveNulls set to false.");
    }
  }

  /**
   * Provides a specialized implementation for the map operation over BoolStorage.
   *
   * @return Computed result or null to fallback to the standard implementation.
   */
  protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
      BoolStorage left,
      boolean rightBoolean,
      boolean rightIsNothing,
      MapOperationProblemAggregator problemAggregator) {
    return null;
  }

  /**
   * Provides a specialized implementation for the map operation over null storage.
   *
   * @return Computed result.
   */
  protected ColumnStorage<Boolean> applySpecializedZipOverNullStorage(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (preserveNulls) {
      return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
    } else {
      throw new IllegalStateException(
          "Cannot apply zip operation over null storage with preserveNulls set to false.");
    }
  }

  /**
   * Provides a specialized implementation for the zip operation over two BoolStorage objects.
   *
   * @return Computed result or null to fallback to the standard implementation.
   */
  protected ColumnBooleanStorage applySpecializedZipOverBoolStorage(
      BoolStorage left, BoolStorage right, MapOperationProblemAggregator problemAggregator) {
    return null;
  }

  protected abstract Boolean applySingle(
      boolean left, boolean isNothing, boolean right, boolean isNothingRight);
}
