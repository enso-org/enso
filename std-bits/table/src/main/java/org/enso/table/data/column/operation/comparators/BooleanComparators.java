package org.enso.table.data.column.operation.comparators;

import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.BinaryOperationBoolean;
import org.enso.table.data.column.operation.unary.NotOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

final class BooleanComparators {
  public static final BinaryOperation<Boolean> EQ =
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

  public static final BinaryOperation<Boolean> NEQ =
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
}
