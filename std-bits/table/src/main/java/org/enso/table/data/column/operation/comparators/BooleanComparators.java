package org.enso.table.data.column.operation.comparators;

import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.BinaryOperationBoolean;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;

public final class BooleanComparators {
  public static final BinaryOperation<Boolean> EQ =
      new BinaryOperationBoolean() {
        @Override
        protected Boolean applySingle(
            boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
          return left == right;
        }

        @Override
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
          return rightBoolean ? left : left.makeNegated();
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
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
          return rightBoolean ? left.makeNegated() : left;
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
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
          return rightBoolean
              ? left.makeNegated()
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
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
          return rightBoolean
              ? new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), true)
              : left.makeNegated();
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
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
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
        protected ColumnBooleanStorage applyMapOverBoolStorage(
            BoolStorage left, boolean rightBoolean, boolean rightIsNothing) {
          return rightBoolean
              ? left
              : new BoolStorage(
                  new BitSet(), left.getIsNothingMap(), Builder.checkSize(left.getSize()), true);
        }
      };
}
