package org.enso.table.data.column.operation.comparators;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationNumeric;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.operation.NumericColumnAdapter;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

abstract class NumericComparators<T> extends BinaryOperationNumeric<T, Boolean> {
  /**
   * An abstract class representing a numeric operation. This class defines the methods that must be
   * implemented by any numeric operation.
   */
  public abstract static class NumericComparator {
    abstract boolean doDouble(
        double a, double b, long ix, MapOperationProblemAggregator problemAggregator);

    abstract boolean doLong(long a, long b, long ix);

    abstract boolean doBigInteger(BigInteger a, BigInteger b, long ix);

    abstract boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix);
  }

  public static final NumericComparator EQUAL_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          problemAggregator.reportFloatingPointEquality(ix);
          return a == b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a == b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) == 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.equals(b);
        }
      };

  public static final NumericComparator NOT_EQUAL_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          problemAggregator.reportFloatingPointEquality(ix);
          return a != b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a != b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) != 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return !a.equals(b);
        }
      };

  public static final NumericComparator GREATER_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a > b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a > b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) > 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.compareTo(b) > 0;
        }
      };

  public static final NumericComparator GREATER_OR_EQUAL_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a >= b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a >= b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) >= 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.compareTo(b) >= 0;
        }
      };

  public static final NumericComparator LESS_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a < b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a < b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) < 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.compareTo(b) < 0;
        }
      };

  public static final NumericComparator LESS_OR_EQUAL_OPERATION =
      new NumericComparator() {
        @Override
        boolean doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a <= b;
        }

        @Override
        boolean doLong(long a, long b, long ix) {
          return a <= b;
        }

        @Override
        boolean doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.compareTo(b) <= 0;
        }

        @Override
        boolean doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.compareTo(b) <= 0;
        }
      };

  static BinaryOperationTyped<Boolean> create(
      StorageType<?> leftType, Object right, NumericComparator comparator) {
    var rightType = storageTypeForObject(right);
    if (leftType instanceof FloatType || rightType instanceof FloatType) {
      return new NumericComparatorsDouble(comparator);
    } else if (leftType instanceof BigDecimalType || rightType instanceof BigDecimalType) {
      return new NumericComparatorsBigDecimal(comparator);
    } else if (leftType instanceof BigIntegerType || rightType instanceof BigIntegerType) {
      return new NumericComparatorsBigInteger(comparator);
    } else if (leftType instanceof IntegerType || rightType instanceof IntegerType) {
      return new NumericComparatorsLong(comparator);
    } else {
      throw new IllegalArgumentException("Unsupported type: " + leftType + " or " + rightType);
    }
  }

  static BinaryOperationTyped<Boolean> create(
      StorageType<?> leftType, Object right, NumericComparator comparator, boolean valueOnOther) {
    var rightType = storageTypeForObject(right);
    if (leftType instanceof FloatType || rightType instanceof FloatType) {
      return new NumericComparatorsDouble(comparator, valueOnOther);
    } else if (leftType instanceof BigDecimalType || rightType instanceof BigDecimalType) {
      return new NumericComparatorsBigDecimal(comparator, valueOnOther);
    } else if (leftType instanceof BigIntegerType || rightType instanceof BigIntegerType) {
      return new NumericComparatorsBigInteger(comparator, valueOnOther);
    } else if (leftType instanceof IntegerType || rightType instanceof IntegerType) {
      return new NumericComparatorsLong(comparator, valueOnOther);
    } else {
      throw new IllegalArgumentException("Unsupported type: " + leftType + " or " + rightType);
    }
  }

  protected final NumericComparator comparator;

  protected NumericComparators(NumericColumnAdapter<T> adapter, NumericComparator comparator) {
    super(adapter, true, BooleanType.INSTANCE);
    this.comparator = comparator;
  }

  protected NumericComparators(
      NumericColumnAdapter<T> adapter, NumericComparator comparator, boolean valueOnOther) {
    super(adapter, true, BooleanType.INSTANCE, valueOnOther);
    this.comparator = comparator;
  }

  @Override
  protected ColumnStorage<Boolean> applyNullMap(
      ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
    return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
  }

  private static class NumericComparatorsDouble extends NumericComparators<Double> {
    public NumericComparatorsDouble(NumericComparator comparator) {
      super(NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, comparator);
    }

    public NumericComparatorsDouble(NumericComparator comparator, boolean valueOnOther) {
      super(NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, comparator, valueOnOther);
    }

    @Override
    protected ColumnStorage<Boolean> innerApplyMap(
        ColumnStorage<Double> left, Double right, MapOperationProblemAggregator problemAggregator) {
      double rightAsDouble = right;
      return StorageIterators.buildOverDoubleStorage(
          (ColumnDoubleStorage) left,
          true,
          BooleanType.INSTANCE.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.appendBoolean(
                  comparator.doDouble(value, rightAsDouble, index, problemAggregator)));
    }

    @Override
    protected ColumnStorage<Boolean> innerApplyZip(
        ColumnStorage<Double> left,
        ColumnStorage<Double> right,
        MapOperationProblemAggregator problemAggregator) {
      return StorageIterators.zipOverDoubleStorages(
          (ColumnDoubleStorage) left,
          (ColumnDoubleStorage) right,
          s -> BooleanType.INSTANCE.makeBuilder(s, problemAggregator),
          true,
          (index, value1, isNothing1, value2, isNothing2) ->
              comparator.doDouble(value1, value2, index, problemAggregator));
    }

    @Override
    protected Boolean doSingle(
        Double left, Double right, long index, MapOperationProblemAggregator problemAggregator) {
      return comparator.doDouble(left, right, index, problemAggregator);
    }
  }

  private static class NumericComparatorsBigDecimal extends NumericComparators<BigDecimal> {
    public NumericComparatorsBigDecimal(NumericComparator comparator) {
      super(NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE, comparator);
    }

    public NumericComparatorsBigDecimal(NumericComparator comparator, boolean valueOnOther) {
      super(NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE, comparator, valueOnOther);
    }

    @Override
    protected Boolean doSingle(
        BigDecimal left,
        BigDecimal right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return comparator.doBigDecimal(left, right, index);
    }
  }

  private static class NumericComparatorsBigInteger extends NumericComparators<BigInteger> {
    public NumericComparatorsBigInteger(NumericComparator comparator) {
      super(NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE, comparator);
    }

    public NumericComparatorsBigInteger(NumericComparator comparator, boolean valueOnOther) {
      super(NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE, comparator, valueOnOther);
    }

    @Override
    protected Boolean doSingle(
        BigInteger left,
        BigInteger right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return comparator.doBigInteger(left, right, index);
    }
  }

  private static class NumericComparatorsLong extends NumericComparators<Long> {
    public NumericComparatorsLong(NumericComparator operation) {
      super(NumericColumnAdapter.LongColumnAdapter.INSTANCE, operation);
    }

    public NumericComparatorsLong(NumericComparator operation, boolean valueOnOther) {
      super(NumericColumnAdapter.LongColumnAdapter.INSTANCE, operation, valueOnOther);
    }

    @Override
    protected ColumnStorage<Boolean> innerApplyMap(
        ColumnStorage<Long> left, Long right, MapOperationProblemAggregator problemAggregator) {
      long rightAsLong = right;
      return StorageIterators.buildOverLongStorage(
          (ColumnLongStorage) left,
          true,
          BooleanType.INSTANCE.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.appendBoolean(comparator.doLong(value, right, index)));
    }

    @Override
    protected ColumnStorage<Boolean> innerApplyZip(
        ColumnStorage<Long> left,
        ColumnStorage<Long> right,
        MapOperationProblemAggregator problemAggregator) {
      return StorageIterators.zipOverLongStorages(
          (ColumnLongStorage) left,
          (ColumnLongStorage) right,
          s -> BooleanType.INSTANCE.makeBuilder(s, problemAggregator),
          true,
          (index, value1, isNothing1, value2, isNothing2) ->
              comparator.doLong(value1, value2, index));
    }

    @Override
    protected Boolean doSingle(
        Long left, Long right, long index, MapOperationProblemAggregator problemAggregator) {
      return comparator.doLong(left, right, index);
    }
  }
}
