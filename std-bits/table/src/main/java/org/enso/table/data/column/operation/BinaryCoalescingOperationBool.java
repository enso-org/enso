package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.error.UnexpectedTypeException;

public abstract class BinaryCoalescingOperationBool extends BinaryCoalescingOperation<Boolean> {
  public static final BinaryCoalescingOperationBool MIN_INSTANCE =
      new BinaryCoalescingOperationBool() {
        @Override
        protected boolean applySingle(boolean left, boolean right) {
          return left && right;
        }

        @Override
        protected ColumnStorage<Boolean> applyMapBoolStorage(BoolStorage left, boolean rightValue) {
          return rightValue
              ? left.fillMissingBoolean(true)
              : BoolStorage.makeConstant(Builder.checkSize(left.getSize()), false);
        }
      };

  public static final BinaryCoalescingOperationBool MAX_INSTANCE =
      new BinaryCoalescingOperationBool() {
        @Override
        protected boolean applySingle(boolean left, boolean right) {
          return left || right;
        }

        @Override
        protected ColumnStorage<Boolean> applyMapBoolStorage(BoolStorage left, boolean rightValue) {
          return rightValue
              ? BoolStorage.makeConstant(Builder.checkSize(left.getSize()), true)
              : left.fillMissingBoolean(false);
        }
      };

  private BinaryCoalescingOperationBool() {
    super(BooleanType.INSTANCE, null);
  }

  @Override
  public ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue) {
    var typedStorage = BooleanType.INSTANCE.asTypedStorage(left);

    if (rightValue == null) {
      return typedStorage;
    }

    if (rightValue instanceof Boolean boolValue) {
      // Special optimised mode
      if (typedStorage instanceof BoolStorage boolStorage) {
        return applyMapBoolStorage(boolStorage, boolValue);
      } else {
        return StorageIterators.buildOverBooleanStorage(
            typedStorage,
            Builder.getForBoolean(typedStorage.getSize()),
            (builder, index, value, isNothing) ->
                builder.appendBoolean(applySingle(value, boolValue)));
      }
    }

    throw new UnexpectedTypeException("a Boolean", rightValue.toString());
  }

  @Override
  public ColumnStorage<Boolean> applyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    var typedStorage = BooleanType.INSTANCE.asTypedStorage(left);

    if (NullType.INSTANCE.isOfType(right.getType())) {
      return typedStorage;
    }

    var typedRightStorage = BooleanType.INSTANCE.asTypedStorage(right);
    return StorageIterators.zipOverBooleanStorages(
        BooleanType.INSTANCE.asTypedStorage(left),
        BooleanType.INSTANCE.asTypedStorage(right),
        Builder::getForBoolean,
        false,
        (index, l, lIsNothing, r, rIsNothing) -> {
          if (lIsNothing) {
            return rIsNothing ? null : r;
          } else if (rIsNothing) {
            return l;
          } else {
            return applySingle(l, r);
          }
        });
  }

  protected abstract boolean applySingle(boolean left, boolean right);

  protected abstract ColumnStorage<Boolean> applyMapBoolStorage(
      BoolStorage left, boolean rightValue);
}
