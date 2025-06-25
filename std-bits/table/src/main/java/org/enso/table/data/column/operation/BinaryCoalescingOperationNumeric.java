package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * A binary coalescing operation for numeric types. This class is used to perform operations on two
 * numeric columns, where the result is a new column that contains the result of the operation.
 *
 * <p>If Double left or right, the result is Double. Else if BigDecimal left or right, the result is
 * BigDecimal. Else if BigInteger left or right, the result is BigInteger. Else Long Long ==> Long
 *
 * @param <T> the type of the elements in the column
 */
public abstract class BinaryCoalescingOperationNumeric<T> extends BinaryOperationNumeric<T, T> {
  /**
   * An abstract class representing a numeric operation. This class defines the methods that must be
   * implemented by any numeric operation.
   */
  public abstract static class NumericOperation {
    abstract double doDouble(double a, double b, long ix);

    abstract long doLong(long a, long b, long ix);

    abstract BigInteger doBigInteger(BigInteger a, BigInteger b, long ix);

    abstract BigDecimal doBigDecimal(BigDecimal a, BigDecimal b, long ix);
  }

  public static final NumericOperation MIN_OPERATION =
      new NumericOperation() {
        @Override
        public double doDouble(double a, double b, long ix) {
          return Math.min(a, b);
        }

        @Override
        public long doLong(long a, long b, long ix) {
          return Math.min(a, b);
        }

        @Override
        public BigInteger doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.min(b);
        }

        @Override
        public BigDecimal doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.min(b);
        }
      };

  public static final NumericOperation MAX_OPERATION =
      new NumericOperation() {
        @Override
        public double doDouble(double a, double b, long ix) {
          return Math.max(a, b);
        }

        @Override
        public long doLong(long a, long b, long ix) {
          return Math.max(a, b);
        }

        @Override
        public BigInteger doBigInteger(BigInteger a, BigInteger b, long ix) {
          return a.max(b);
        }

        @Override
        public BigDecimal doBigDecimal(BigDecimal a, BigDecimal b, long ix) {
          return a.max(b);
        }
      };

  public static BinaryOperationTyped<?> create(
      StorageType<?> leftType, Object right, NumericOperation operation) {
    var rightType = storageTypeForObject(right);
    if (leftType instanceof FloatType || rightType instanceof FloatType) {
      return new BinaryCoalescingOperationDouble(operation);
    } else if (leftType instanceof BigDecimalType || rightType instanceof BigDecimalType) {
      return new BinaryCoalescingOperationBigDecimal(operation);
    } else if (leftType instanceof BigIntegerType || rightType instanceof BigIntegerType) {
      return new BinaryCoalescingOperationBigInteger(operation);
    } else if (leftType instanceof IntegerType || rightType instanceof IntegerType) {
      return new BinaryCoalescingOperationLong(operation);
    } else {
      throw new IllegalArgumentException("Unsupported type: " + leftType);
    }
  }

  protected final NumericOperation operation;

  protected BinaryCoalescingOperationNumeric(
      NumericColumnAdapter<T> adapter, StorageType<T> returnType, NumericOperation operation) {
    super(adapter, false, returnType);
    this.operation = operation;
  }

  @Override
  protected ColumnStorage<T> applyNullMap(
      ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
    return adapter.asTypedStorage(left);
  }

  private static class BinaryCoalescingOperationDouble
      extends BinaryCoalescingOperationNumeric<Double> {
    public BinaryCoalescingOperationDouble(NumericOperation operation) {
      super(NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, FloatType.FLOAT_64, operation);
    }

    @Override
    protected ColumnStorage<Double> innerApplyMap(
        ColumnStorage<Double> left, Double right, MapOperationProblemAggregator problemAggregator) {
      double rightAsDouble = right;
      return StorageIterators.buildOverDoubleStorage(
          (ColumnDoubleStorage) left,
          false,
          FloatType.FLOAT_64.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.appendDouble(isNothing ? right : operation.doDouble(value, right, index)));
    }

    @Override
    protected ColumnStorage<Double> innerApplyZip(
        ColumnStorage<Double> left,
        ColumnStorage<Double> right,
        MapOperationProblemAggregator problemAggregator) {
      return StorageIterators.zipOverDoubleStorages(
          (ColumnDoubleStorage) left,
          (ColumnDoubleStorage) right,
          s -> FloatType.FLOAT_64.makeBuilder(s, problemAggregator),
          false,
          (index, value1, isNothing1, value2, isNothing2) -> {
            if (isNothing1 && isNothing2) {
              return null;
            } else if (isNothing1) {
              return value2;
            } else if (isNothing2) {
              return value1;
            } else {
              return operation.doDouble(value1, value2, index);
            }
          });
    }

    @Override
    protected Double doSingle(
        Double left, Double right, long index, MapOperationProblemAggregator problemAggregator) {
      if (left == null) {
        return right;
      } else if (right == null) {
        return left;
      } else {
        return operation.doDouble(left, right, index);
      }
    }
  }

  private static class BinaryCoalescingOperationBigDecimal
      extends BinaryCoalescingOperationNumeric<BigDecimal> {
    public BinaryCoalescingOperationBigDecimal(NumericOperation operation) {
      super(
          NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE,
          BigDecimalType.INSTANCE,
          operation);
    }

    @Override
    protected BigDecimal doSingle(
        BigDecimal left,
        BigDecimal right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return left == null
          ? right
          : (right == null ? left : operation.doBigDecimal(left, right, index));
    }
  }

  private static class BinaryCoalescingOperationBigInteger
      extends BinaryCoalescingOperationNumeric<BigInteger> {
    public BinaryCoalescingOperationBigInteger(NumericOperation operation) {
      super(
          NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE,
          BigIntegerType.INSTANCE,
          operation);
    }

    @Override
    protected BigInteger doSingle(
        BigInteger left,
        BigInteger right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return left == null
          ? right
          : (right == null ? left : operation.doBigInteger(left, right, index));
    }
  }

  private static class BinaryCoalescingOperationLong
      extends BinaryCoalescingOperationNumeric<Long> {
    public BinaryCoalescingOperationLong(NumericOperation operation) {
      super(NumericColumnAdapter.LongColumnAdapter.INSTANCE, IntegerType.INT_64, operation);
    }

    @Override
    protected ColumnStorage<Long> innerApplyMap(
        ColumnStorage<Long> left, Long right, MapOperationProblemAggregator problemAggregator) {
      long rightAsLong = right;
      return StorageIterators.buildOverLongStorage(
          (ColumnLongStorage) left,
          false,
          IntegerType.INT_64.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.appendLong(isNothing ? right : operation.doLong(value, right, index)));
    }

    @Override
    protected ColumnStorage<Long> innerApplyZip(
        ColumnStorage<Long> left,
        ColumnStorage<Long> right,
        MapOperationProblemAggregator problemAggregator) {
      return StorageIterators.zipOverLongStorages(
          (ColumnLongStorage) left,
          (ColumnLongStorage) right,
          s -> IntegerType.INT_64.makeBuilder(s, problemAggregator),
          true,
          (index, value1, isNothing1, value2, isNothing2) -> {
            if (isNothing1 && isNothing2) {
              return null;
            } else if (isNothing1) {
              return value2;
            } else if (isNothing2) {
              return value1;
            } else {
              return operation.doLong(value1, value2, index);
            }
          });
    }

    @Override
    protected Long doSingle(
        Long left, Long right, long index, MapOperationProblemAggregator problemAggregator) {
      if (left == null) {
        return right;
      } else if (right == null) {
        return left;
      } else {
        return operation.doLong(left, right, index);
      }
    }
  }
}
