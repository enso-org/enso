package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBoolean;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.operation.unary.NotOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.util.ImmutableBitSet;

final class BooleanComparators {
  public static final BinaryOperationTyped<Boolean> EQ =
      new BinaryOperationBoolean(true, false, false) {
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

  public static final BinaryOperationTyped<Boolean> NEQ =
      new BinaryOperationBoolean(true, false, true) {
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

  public static final BinaryOperationTyped<Boolean> LT =
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
          var size = (int) left.getSize();
          return rightBoolean
              ? NotOperation.applySpecializedBoolStorage(left)
              : new BoolStorage(
                  ImmutableBitSet.allFalse(size),
                  left.getValidityMap(),
                  Builder.checkSize(size),
                  false,
                  null);
        }
      };

  public static final BinaryOperationTyped<Boolean> LTE =
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
          var size = (int) left.getSize();
          return rightBoolean
              ? new BoolStorage(
                  ImmutableBitSet.allFalse(size),
                  left.getValidityMap(),
                  Builder.checkSize(size),
                  true,
                  null)
              : NotOperation.applySpecializedBoolStorage(left);
        }
      };

  public static final BinaryOperationTyped<Boolean> GT =
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
          var size = Builder.checkSize(left.getSize());
          return rightBoolean
              ? new BoolStorage(
                  ImmutableBitSet.allFalse(size), left.getValidityMap(), size, false, null)
              : left;
        }
      };

  public static final BinaryOperationTyped<Boolean> GTE =
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
          var size = (int) left.getSize();
          return rightBoolean
              ? left
              : new BoolStorage(
                  ImmutableBitSet.allFalse(size),
                  left.getValidityMap(),
                  Builder.checkSize(left.getSize()),
                  true,
                  null);
        }
      };
}
