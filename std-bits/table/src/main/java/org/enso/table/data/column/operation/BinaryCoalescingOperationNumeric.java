package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageFacade;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.BlackholeProblemAggregator;

/**
 * A binary coalescing operation for numeric types. This class is used to perform operations on two
 * numeric columns, where the result is a new column that contains the result of the operation.
 *
 * <p>If Double left or right, the result is Double. Else if BigDecimal left or right, the result is
 * BigDecimal. Else if BigInteger left or right, the result is BigInteger. Else Long Long ==> Long
 *
 * @param <T> the type of the elements in the column
 */
public abstract class BinaryCoalescingOperationNumeric<T> implements BinaryOperation<T> {
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

  public static BinaryOperation<?> create(
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

  private static StorageType<?> storageTypeForObject(Object right) {
    if (right == null) {
      return NullType.INSTANCE;
    }

    if (right instanceof Column rightColumn) {
      return rightColumn.getStorage().getType();
    }

    return StorageType.forBoxedItem(right, PreciseTypeOptions.DEFAULT);
  }

  protected final StorageType<T> validType;
  protected final NumericOperation operation;
  protected final StorageType<?>[] validInputs;

  protected BinaryCoalescingOperationNumeric(
      StorageType<T> validType, NumericOperation operation, StorageType<?>... validInputs) {
    this.validType = validType;
    this.operation = operation;
    this.validInputs = validInputs;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var leftType = left.getType();
    if (validType.isOfType(leftType)) {
      return true;
    }

    for (var validInput : validInputs) {
      if (validInput.isOfType(leftType)) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null) && canApplyMap(right, null);
  }

  @Override
  public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue) {
    if (rightValue == null) {
      return asTypedStorage(left);
    }

    T rightValueTyped = validType.valueAsType(rightValue);
    if (rightValueTyped == null) {
      throw new IllegalArgumentException(
          "Unsupported right value type " + rightValue.getClass() + ".");
    }

    return innerApplyMap(asTypedStorage(left), rightValueTyped);
  }

  @Override
  public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    if (NullType.INSTANCE.isOfType(right.getType())) {
      return validType.asTypedStorage(left);
    }

    return innerApplyZip(asTypedStorage(left), asTypedStorage(right));
  }

  protected abstract ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage);

  protected abstract ColumnStorage<T> innerApplyMap(ColumnStorage<T> left, T right);

  protected abstract ColumnStorage<T> innerApplyZip(ColumnStorage<T> left, ColumnStorage<T> right);

  private static class BinaryCoalescingOperationDouble
      extends BinaryCoalescingOperationNumeric<Double> {
    public BinaryCoalescingOperationDouble(NumericOperation operation) {
      super(
          FloatType.FLOAT_64,
          operation,
          BigDecimalType.INSTANCE,
          BigIntegerType.INSTANCE,
          IntegerType.INT_64);
    }

    @Override
    protected ColumnDoubleStorage asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case FloatType floatType -> floatType.asTypedStorage(storage);
        case BigDecimalType bigDecimalType -> DoubleStorageFacade.forBigDecimal(
            bigDecimalType.asTypedStorage(storage));
        case BigIntegerType bigIntegerType -> DoubleStorageFacade.forBigInteger(
            bigIntegerType.asTypedStorage(storage));
        case IntegerType integerType -> DoubleStorageFacade.forLong(
            integerType.asTypedStorage(storage));
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }

    @Override
    protected ColumnStorage<Double> innerApplyMap(ColumnStorage<Double> left, Double right) {
      double rightAsDouble = right;
      return StorageIterators.buildOverDoubleStorage(
          (ColumnDoubleStorage) left,
          false,
          FloatType.FLOAT_64.makeBuilder(left.getSize(), BlackholeProblemAggregator.INSTANCE),
          (builder, index, value, isNothing) ->
              builder.appendDouble(isNothing ? right : operation.doDouble(value, right, index)));
    }

    @Override
    protected ColumnStorage<Double> innerApplyZip(
        ColumnStorage<Double> left, ColumnStorage<Double> right) {
      return StorageIterators.zipOverDoubleStorages(
          (ColumnDoubleStorage) left,
          (ColumnDoubleStorage) right,
          s -> FloatType.FLOAT_64.makeBuilder(s, BlackholeProblemAggregator.INSTANCE),
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
  }

  private abstract static class BinaryCoalescingOperationBigNumber<T>
      extends BinaryCoalescingOperationNumeric<T> {
    protected BinaryCoalescingOperationBigNumber(
        StorageType<T> validType, NumericOperation operation, StorageType<?>... validInputs) {
      super(validType, operation, validInputs);
    }

    @Override
    protected ColumnStorage<T> innerApplyMap(ColumnStorage<T> left, T right) {
      return StorageIterators.mapOverStorage(
          left,
          false,
          validType.makeBuilder(left.getSize(), BlackholeProblemAggregator.INSTANCE),
          (index, value) -> value == null ? right : doSingle(value, right, index));
    }

    @Override
    protected ColumnStorage<T> innerApplyZip(ColumnStorage<T> left, ColumnStorage<T> right) {
      return StorageIterators.zipOverStorages(
          left,
          right,
          size -> validType.makeBuilder(size, BlackholeProblemAggregator.INSTANCE),
          false,
          (index, x, y) -> {
            if (x == null) {
              return y;
            } else {
              return y == null ? x : doSingle(x, y, index);
            }
          });
    }

    protected abstract T doSingle(T left, T right, long index);
  }

  private static class BinaryCoalescingOperationBigDecimal
      extends BinaryCoalescingOperationBigNumber<BigDecimal> {
    public BinaryCoalescingOperationBigDecimal(NumericOperation operation) {
      super(BigDecimalType.INSTANCE, operation, BigIntegerType.INSTANCE, IntegerType.INT_64);
    }

    @Override
    protected ColumnStorage<BigDecimal> asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case BigDecimalType bigDecimalType -> bigDecimalType.asTypedStorage(storage);
        case BigIntegerType bigIntegerType -> new ColumnStorageFacade<>(
            bigIntegerType.asTypedStorage(storage), BigDecimal::new);
        case IntegerType integerType -> new ColumnStorageFacade<>(
            integerType.asTypedStorage(storage), BigDecimal::valueOf);
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }

    @Override
    protected BigDecimal doSingle(BigDecimal left, BigDecimal right, long index) {
      return operation.doBigDecimal(left, right, index);
    }
  }

  private static class BinaryCoalescingOperationBigInteger
      extends BinaryCoalescingOperationBigNumber<BigInteger> {
    public BinaryCoalescingOperationBigInteger(NumericOperation operation) {
      super(BigIntegerType.INSTANCE, operation, IntegerType.INT_64);
    }

    @Override
    protected ColumnStorage<BigInteger> asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case BigIntegerType bigIntegerType -> bigIntegerType.asTypedStorage(storage);
        case IntegerType integerType -> new ColumnStorageFacade<>(
            integerType.asTypedStorage(storage), BigInteger::valueOf);
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }

    @Override
    protected BigInteger doSingle(BigInteger left, BigInteger right, long index) {
      return operation.doBigInteger(left, right, index);
    }
  }

  private static class BinaryCoalescingOperationLong
      extends BinaryCoalescingOperationNumeric<Long> {
    public BinaryCoalescingOperationLong(NumericOperation operation) {
      super(IntegerType.INT_64, operation);
    }

    @Override
    protected ColumnLongStorage asTypedStorage(ColumnStorage<?> storage) {
      if (storage.getType() instanceof IntegerType integerType) {
        return integerType.asTypedStorage(storage);
      }
      throw new IllegalArgumentException("Unsupported storage type: " + storage.getType());
    }

    @Override
    protected ColumnStorage<Long> innerApplyMap(ColumnStorage<Long> left, Long right) {
      long rightAsLong = right;
      return StorageIterators.buildOverLongStorage(
          (ColumnLongStorage) left,
          false,
          IntegerType.INT_64.makeBuilder(left.getSize(), BlackholeProblemAggregator.INSTANCE),
          (builder, index, value, isNothing) ->
              builder.appendLong(isNothing ? right : operation.doLong(value, right, index)));
    }

    @Override
    protected ColumnStorage<Long> innerApplyZip(
        ColumnStorage<Long> left, ColumnStorage<Long> right) {
      return StorageIterators.zipOverLongStorages(
          (ColumnLongStorage) left,
          (ColumnLongStorage) right,
          s -> validType.makeBuilder(s, BlackholeProblemAggregator.INSTANCE),
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
  }
}
