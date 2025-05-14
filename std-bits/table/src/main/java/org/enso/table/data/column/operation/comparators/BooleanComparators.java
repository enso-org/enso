package org.enso.table.data.column.operation.comparators;

import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.BinaryOperationBoolean;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.unary.NotOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.util.BitSets;

public final class BooleanComparators {
  public static final BinaryOperation<Boolean> EQ =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return left == right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean ? left : NotOperation.applySpecializedBoolStorage(left);
        }
      };

  public static final BinaryOperation<Boolean> NEQ =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return left != right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean ? NotOperation.applySpecializedBoolStorage(left) : left;
        }
      };

  public static final BinaryOperation<Boolean> LT =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return !left && right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean
              ? NotOperation.applySpecializedBoolStorage(left)
              : new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), false);
        }
      };

  public static final BinaryOperation<Boolean> LTE =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return !left || right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean
              ? new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), true)
              : NotOperation.applySpecializedBoolStorage(left);
        }
      };

  public static final BinaryOperation<Boolean> GT =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return left && !right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean
              ? new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), false)
              : left;
        }
      };

  public static final BinaryOperation<Boolean> GTE =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return left || !right;
        }

        @Override
        protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
            BoolStorage left,
            boolean rightBoolean,
            boolean rightIsNothing,
            MapOperationProblemAggregator problemAggregator) {
          return rightBoolean
              ? left
              : new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), true);
        }
      };

  public static final BinaryOperation<Boolean> AND = new BooleanAndOperation();

  /**
   * Logical AND with support for Nulls:
   * True && True = True
   * True && False = False
   * False && True = False
   * False && False = False
   * True && Null = Null
   * Null && True = Null
   * False && Null = False
   * Null && False = False
   */
  private static class BooleanAndOperation extends BinaryOperationBoolean {
    private BooleanAndOperation() {
      super(false);
    }

    @Override
    protected Boolean applySingle(boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
      if (isNothing) {
        return isNothingRight || right ? null : false;
      } else if (isNothingRight) {
        return left ? null : false;
      } else {
        return left && right;
      }
    }

    @Override
    protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(BoolStorage left, boolean rightBoolean, boolean rightIsNothing, MapOperationProblemAggregator problemAggregator) {
      int size = (int) left.getSize();
      if (!rightIsNothing) {
        return rightBoolean ? left : BoolStorage.makeConstant(size, false);
      }

      BitSet values = left.getValues();
      if (left.isNegated()) {
        var newMissing = new BitSet(size);
        newMissing.flip(0, size);
        newMissing.xor(values);
        return new BoolStorage(values, newMissing, size, true);
      } else {
        var newMissing = left.getIsNothingMap().get(0, size);
        newMissing.or(values);
        return new BoolStorage(new BitSet(), newMissing, size, false);
      }
    }

    @Override
    protected ColumnBooleanStorage applySpecializedZipOverBoolStorage(BoolStorage left, BoolStorage right, MapOperationProblemAggregator problemAggregator) {
      int size = (int) left.getSize();
      int rightSize = (int) right.getSize();
      BitSet values = left.getValues();

      // Compute the output set
      BitSet out = right.getValues().get(0, size);
      boolean negated;
      if (left.isNegated()) {
        if (right.isNegated()) {
          out.or(values);
          negated = true;
        } else {
          out.andNot(values);
          negated = false;
        }
      } else if (right.isNegated()) {
        out.flip(0, size);
        out.and(values);
        negated = false;
      } else {
        out.and(values);
        negated = false;
      }

      BitSet isNothing = BitSets.makeDuplicate(left.getIsNothingMap());
      isNothing.or(right.getIsNothingMap());
      if (size > rightSize) {
        isNothing.set(rightSize, size);
      }
      int current = isNothing.nextSetBit(0);
      while (current != -1) {
        Boolean a = left.getItemBoxed(current);
        Boolean b = (current < rightSize) ? right.getItemBoxed(current) : null;
        if (a == Boolean.FALSE || b == Boolean.FALSE) {
          isNothing.clear(current);
          out.set(current, negated);
        }
        current = isNothing.nextSetBit(current + 1);
      }

      return new BoolStorage(out, isNothing, size, negated);
    }
  }
}
