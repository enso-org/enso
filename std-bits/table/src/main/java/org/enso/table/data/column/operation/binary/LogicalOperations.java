package org.enso.table.data.column.operation.binary;

import java.util.BitSet;
import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.BinaryOperationBoolean;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.util.BitSets;

/**
 * This class contains logical operations that can be applied to columns.
 *
 * <p>It includes operations like AND and OR, which can be used to perform logical operations on
 * boolean columns.
 */
public final class LogicalOperations {
  /** The logical AND operation instance. */
  public static final BinaryOperation<Boolean> AND = new BooleanAndOperation();

  /** The logical OR operation instance. */
  public static final BinaryOperation<Boolean> OR = new BooleanOrOperation();

  /**
   * Logical AND with support for Nulls:
   *
   * <p>
   *
   * <ul>
   *   <li>True && True = True
   *   <li>True && False = False
   *   <li>False && True = False
   *   <li>False && False = False
   *   <li>True && Null = Null
   *   <li>Null && True = Null
   *   <li>False && Null = False
   *   <li>Null && False = False
   * </ul>
   */
  private static class BooleanAndOperation extends BinaryOperationBoolean {
    private BooleanAndOperation() {
      super(false, true);
    }

    @Override
    protected Boolean applySingle(
        boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
      if (isNothing) {
        return isNothingRight || right ? null : false;
      } else if (isNothingRight) {
        return left ? null : false;
      } else {
        return left && right;
      }
    }

    @Override
    protected ColumnStorage<Boolean> applySpecializedMapOverNullStorage(
        ColumnStorage<?> left,
        boolean rightBoolean,
        boolean rightIsNothing,
        MapOperationProblemAggregator problemAggregator) {
      return rightIsNothing || rightBoolean
          ? BoolBuilder.makeEmpty(left.getSize())
          : BoolBuilder.makeConstant(left.getSize(), false);
    }

    @Override
    protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
        BoolStorage left,
        boolean rightBoolean,
        boolean rightIsNothing,
        MapOperationProblemAggregator problemAggregator) {
      if (!rightIsNothing) {
        return rightBoolean ? left : BoolBuilder.makeConstant(left.getSize(), false);
      }

      int size = (int) left.getSize();
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
    protected ColumnStorage<Boolean> applySpecializedZipOverNullStorage(
        ColumnStorage<?> left,
        ColumnStorage<?> right,
        MapOperationProblemAggregator problemAggregator) {
      return super.applyMap(right, null, problemAggregator);
    }

    @Override
    protected ColumnBooleanStorage applySpecializedZipOverBoolStorage(
        BoolStorage left, BoolStorage right, MapOperationProblemAggregator problemAggregator) {
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

  /**
   * Logical OR with support for Nulls:
   *
   * <p>
   *
   * <ul>
   *   <li>True || True = True
   *   <li>True || False = False
   *   <li>False || True = False
   *   <li>False || False = False
   *   <li>True || Null = True
   *   <li>Null || True = True
   *   <li>False || Null = Null
   *   <li>Null || False = Null
   * </ul>
   */
  private static class BooleanOrOperation extends BinaryOperationBoolean {
    private BooleanOrOperation() {
      super(false, true);
    }

    @Override
    protected Boolean applySingle(
        boolean left, boolean isNothing, boolean right, boolean isNothingRight) {
      if (isNothing) {
        return isNothingRight || !right ? null : true;
      } else if (isNothingRight) {
        return left ? true : null;
      } else {
        return left || right;
      }
    }

    @Override
    protected ColumnStorage<Boolean> applySpecializedMapOverNullStorage(
        ColumnStorage<?> left,
        boolean rightBoolean,
        boolean rightIsNothing,
        MapOperationProblemAggregator problemAggregator) {
      return rightIsNothing || !rightBoolean
          ? BoolBuilder.makeEmpty(left.getSize())
          : BoolBuilder.makeConstant(left.getSize(), true);
    }

    @Override
    protected ColumnBooleanStorage applySpecializedMapOverBoolStorage(
        BoolStorage left,
        boolean rightBoolean,
        boolean rightIsNothing,
        MapOperationProblemAggregator problemAggregator) {
      if (!rightIsNothing) {
        return rightBoolean ? BoolBuilder.makeConstant(left.getSize(), true) : left;
      }

      int size = (int) left.getSize();
      BitSet values = left.getValues();
      if (left.isNegated()) {
        var newMissing = left.getIsNothingMap().get(0, size);
        newMissing.or(values);
        return new BoolStorage(new BitSet(), newMissing, size, true);
      } else {
        var newMissing = new BitSet(size);
        newMissing.flip(0, size);
        newMissing.xor(values);
        return new BoolStorage(values, newMissing, size, false);
      }
    }

    @Override
    protected ColumnStorage<Boolean> applySpecializedZipOverNullStorage(
        ColumnStorage<?> left,
        ColumnStorage<?> right,
        MapOperationProblemAggregator problemAggregator) {
      return super.applyMap(right, null, problemAggregator);
    }

    @Override
    protected ColumnBooleanStorage applySpecializedZipOverBoolStorage(
        BoolStorage left, BoolStorage right, MapOperationProblemAggregator problemAggregator) {
      int size = (int) left.getSize();
      int rightSize = (int) right.getSize();
      BitSet values = left.getValues();

      // Compute the output set
      BitSet out = right.getValues().get(0, size);
      boolean negated;
      if (left.isNegated()) {
        if (right.isNegated()) {
          out.and(values);
          negated = true;
        } else {
          out.flip(0, size);
          out.and(values);
          negated = true;
        }
      } else if (right.isNegated()) {
        out.flip(0, size);
        out.or(values);
        negated = false;
      } else {
        out.or(values);
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
        if (a == Boolean.TRUE || b == Boolean.TRUE) {
          isNothing.clear(current);
          out.set(current, !negated);
        }
        current = isNothing.nextSetBit(current + 1);
      }

      return new BoolStorage(out, isNothing, size, negated);
    }
  }
}
