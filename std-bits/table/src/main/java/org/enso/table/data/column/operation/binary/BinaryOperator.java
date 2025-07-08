package org.enso.table.data.column.operation.binary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationNull;
import org.enso.table.data.column.operation.BinaryOperationNumeric;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.operation.NumericColumnAdapter;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.text.TextConcatenate;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.NumericType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * Support binary operator - provides the framework for numeric operators, and an entry point for
 * all.
 */
public abstract class BinaryOperator<T> extends BinaryOperationNumeric<T, T> {
  /**
   * An abstract class representing a numeric operation. This class defines the methods that must be
   * implemented by any numeric operation.
   */
  public abstract static class NumericOperation {
    abstract Double doDouble(
        double a, double b, long ix, MapOperationProblemAggregator problemAggregator);

    abstract Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator);

    abstract BigInteger doBigInteger(
        BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator);

    abstract BigDecimal doBigDecimal(
        BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator);
  }

  private static final NumericOperation ADDITION =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a + b;
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          try {
            return Math.addExact(a, b);
          } catch (ArithmeticException e) {
            problemAggregator.reportOverflow(IntegerType.INT_64, a, "+", b);
            return null;
          }
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.add(b);
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.add(b);
        }
      };

  private static final NumericOperation SUBTRACTION =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a - b;
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          try {
            return Math.subtractExact(a, b);
          } catch (ArithmeticException e) {
            problemAggregator.reportOverflow(IntegerType.INT_64, a, "-", b);
            return null;
          }
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.subtract(b);
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.subtract(b);
        }
      };

  private static final NumericOperation MULTIPLY =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a * b;
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          try {
            return Math.multiplyExact(a, b);
          } catch (ArithmeticException e) {
            problemAggregator.reportOverflow(IntegerType.INT_64, a, "*", b);
            return null;
          }
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.multiply(b);
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          return a.multiply(b);
        }
      };

  private static final NumericOperation MODULUS =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b == 0.0) {
            problemAggregator.reportDivisionByZero(ix);
          }
          return a % b;
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b == 0) {
            problemAggregator.reportDivisionByZero(ix);
            return null;
          }

          return a % b;
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b.equals(BigInteger.ZERO)) {
            problemAggregator.reportDivisionByZero(ix);
            return null;
          }

          return a.mod(b);
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b.equals(BigDecimal.ZERO)) {
            problemAggregator.reportDivisionByZero(ix);
            return null;
          }

          return a.remainder(b);
        }
      };

  private static final NumericOperation DIVIDE =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b == 0.0) {
            problemAggregator.reportDivisionByZero(ix);
          }
          return a / b;
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          throw new IllegalStateException("Long division is not supported. Should use Double.");
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          throw new IllegalStateException(
              "BigInteger division is not supported. Should use Double.");
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          if (b.equals(BigDecimal.ZERO)) {
            problemAggregator.reportDivisionByZero(ix);
            return null;
          }

          return a.divide(b, MathContext.DECIMAL128);
        }
      };

  private static final NumericOperation POWER =
      new NumericOperation() {
        @Override
        Double doDouble(
            double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
          return Math.pow(a, b);
        }

        @Override
        Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
          throw new IllegalStateException("Long power is not supported. Should use Double.");
        }

        @Override
        BigInteger doBigInteger(
            BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
          throw new IllegalStateException("BigInteger power is not supported. Should use Double.");
        }

        @Override
        BigDecimal doBigDecimal(
            BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
          throw new IllegalStateException("BigDecimal power is not supported. Should use Double.");
        }
      };

  /**
   * Create a binary operation for addition.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs addition or concatenation
   */
  public static BinaryOperationTyped<?> add(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NumericType nt -> createNumeric(leftStorage.getType(), right, ADDITION);
      case TextType tt -> TextConcatenate.INSTANCE;
      case NullType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield switch (rightType) {
          case NullType rnt -> BinaryOperationNull.INSTANCE;
          case NumericType rnt -> createNumeric(leftStorage.getType(), right, ADDITION);
          case TextType rtt -> TextConcatenate.INSTANCE;
          default -> null;
        };
      }
      default -> null;
    };
  }

  /**
   * Create a binary operation for subtraction.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs subtraction
   */
  public static BinaryOperationTyped<?> minus(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NumericType nt -> createNumeric(leftStorage.getType(), right, SUBTRACTION);
      case DateTimeType dtt -> DateTimeSubtraction.DATE_TIME;
      case TimeOfDayType todt -> DateTimeSubtraction.TIME_OF_DAY;
      case NullType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield switch (rightType) {
          case NullType rnt -> BinaryOperationNull.INSTANCE;
          case DateTimeType dtt -> DateTimeSubtraction.DATE_TIME;
          case TimeOfDayType todt -> DateTimeSubtraction.TIME_OF_DAY;
          case NumericType rnt -> createNumeric(leftStorage.getType(), right, SUBTRACTION);
          default -> null;
        };
      }
      default -> null;
    };
  }

  /**
   * Create a binary operation for multiplication.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs multiplication
   */
  public static BinaryOperationTyped<?> multiply(Column left, Object right) {
    return makeNumericBinaryOperation(left, right, MULTIPLY);
  }

  /**
   * Create a binary operation for modulus.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs multiplication
   */
  public static BinaryOperationTyped<?> modulus(Column left, Object right) {
    return makeNumericBinaryOperation(left, right, MODULUS);
  }

  /**
   * Create a binary operation for division.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs division
   */
  public static BinaryOperationTyped<?> divide(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case BigDecimalType bdt -> new BinaryOperatorBigDecimal(DIVIDE);
      case NumericType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield rightType instanceof BigDecimalType bdt
            ? new BinaryOperatorBigDecimal(DIVIDE)
            : new BinaryOperatorDouble(DIVIDE);
      }
      case NullType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield switch (rightType) {
          case NullType rnt -> BinaryOperationNull.INSTANCE;
          case BigDecimalType bdt -> new BinaryOperatorBigDecimal(DIVIDE);
          case NumericType rnt -> new BinaryOperatorDouble(DIVIDE);
          default -> null;
        };
      }
      default -> null;
    };
  }

  /**
   * Create a binary operation for power.
   *
   * @param left the left column
   * @param right the right value (can be a column or a scalar)
   * @return a BinaryOperation that performs division
   */
  public static BinaryOperationTyped<?> power(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NumericType nt -> new BinaryOperatorDouble(POWER);
      case NullType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield switch (rightType) {
          case NullType rnt -> BinaryOperationNull.INSTANCE;
          case NumericType rnt -> new BinaryOperatorDouble(POWER);
          default -> null;
        };
      }
      default -> null;
    };
  }

  private static BinaryOperationTyped<?> makeNumericBinaryOperation(
      Column left, Object right, NumericOperation operation) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NumericType nt -> createNumeric(leftStorage.getType(), right, operation);
      case NullType nt -> {
        // Work out based on the RHS
        var rightType = storageTypeForObject(right);
        yield switch (rightType) {
          case NullType rnt -> BinaryOperationNull.INSTANCE;
          case NumericType rnt -> createNumeric(leftStorage.getType(), right, operation);
          default -> null;
        };
      }
      default -> null;
    };
  }

  static BinaryOperationTyped<?> createNumeric(
      StorageType<?> leftType, Object right, NumericOperation operation) {
    var rightType = storageTypeForObject(right);
    if (leftType instanceof BigDecimalType || rightType instanceof BigDecimalType) {
      return new BinaryOperatorBigDecimal(operation);
    } else if (leftType instanceof FloatType || rightType instanceof FloatType) {
      return new BinaryOperatorDouble(operation);
    } else if (leftType instanceof BigIntegerType || rightType instanceof BigIntegerType) {
      return new BinaryOperatorBigInteger(operation);
    } else if (leftType instanceof IntegerType || rightType instanceof IntegerType) {
      return new BinaryOperatorLong(operation);
    } else {
      throw new IllegalArgumentException("Unsupported type: " + leftType + " or " + rightType);
    }
  }

  protected final NumericOperation operation;

  protected BinaryOperator(NumericColumnAdapter<T> adapter, NumericOperation operation) {
    super(adapter, true, adapter.getValidType());
    this.operation = operation;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object right) {
    if (left.getType() instanceof NullType) {
      return true; // We can apply null map to any right value
    }
    return super.canApplyMap(left, right);
  }

  @Override
  public ColumnStorage<T> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType) {
      return applyNullMap(left, problemAggregator);
    }

    return super.applyZip(left, right, problemAggregator);
  }

  private static class BinaryOperatorDouble extends BinaryOperator<Double> {
    public BinaryOperatorDouble(NumericOperation operation) {
      super(NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, operation);
    }

    @Override
    protected ColumnStorage<Double> applyNullMap(
        ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
      return Builder.makeEmpty(FloatType.FLOAT_64, left.getSize());
    }

    @Override
    protected ColumnStorage<Double> innerApplyMap(
        ColumnStorage<Double> left, Double right, MapOperationProblemAggregator problemAggregator) {
      double rightAsDouble = right;
      return StorageIterators.buildOverDoubleStorage(
          (ColumnDoubleStorage) left,
          true,
          FloatType.FLOAT_64.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.appendDouble(
                  operation.doDouble(value, rightAsDouble, index, problemAggregator)));
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
          true,
          (index, value1, isNothing1, value2, isNothing2) ->
              operation.doDouble(value1, value2, index, problemAggregator));
    }

    @Override
    protected Double doSingle(
        Double left, Double right, long index, MapOperationProblemAggregator problemAggregator) {
      return operation.doDouble(left, right, index, problemAggregator);
    }
  }

  private static class BinaryOperatorBigDecimal extends BinaryOperator<BigDecimal> {
    public BinaryOperatorBigDecimal(NumericOperation operation) {
      super(NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE, operation);
    }

    @Override
    protected ColumnStorage<BigDecimal> applyNullMap(
        ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
      return Builder.makeEmpty(BigDecimalType.INSTANCE, left.getSize());
    }

    @Override
    protected BigDecimal doSingle(
        BigDecimal left,
        BigDecimal right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return operation.doBigDecimal(left, right, index, problemAggregator);
    }
  }

  private static class BinaryOperatorBigInteger extends BinaryOperator<BigInteger> {
    public BinaryOperatorBigInteger(NumericOperation operation) {
      super(NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE, operation);
    }

    @Override
    protected ColumnStorage<BigInteger> applyNullMap(
        ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
      return Builder.makeEmpty(BigIntegerType.INSTANCE, left.getSize());
    }

    @Override
    protected BigInteger doSingle(
        BigInteger left,
        BigInteger right,
        long index,
        MapOperationProblemAggregator problemAggregator) {
      return operation.doBigInteger(left, right, index, problemAggregator);
    }
  }

  private static class BinaryOperatorLong extends BinaryOperator<Long> {
    public BinaryOperatorLong(NumericOperation operation) {
      super(NumericColumnAdapter.LongColumnAdapter.INSTANCE, operation);
    }

    @Override
    protected ColumnStorage<Long> applyNullMap(
        ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator) {
      return Builder.makeEmpty(IntegerType.INT_64, left.getSize());
    }

    @Override
    protected ColumnStorage<Long> innerApplyMap(
        ColumnStorage<Long> left, Long right, MapOperationProblemAggregator problemAggregator) {
      long rightAsLong = right;
      return StorageIterators.buildOverLongStorage(
          (ColumnLongStorage) left,
          true,
          IntegerType.INT_64.makeBuilder(left.getSize(), problemAggregator),
          (builder, index, value, isNothing) ->
              builder.append(operation.doLong(value, rightAsLong, index, problemAggregator)));
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
          (index, value1, isNothing1, value2, isNothing2) ->
              operation.doLong(value1, value2, index, problemAggregator));
    }

    @Override
    protected Long doSingle(
        Long left, Long right, long index, MapOperationProblemAggregator problemAggregator) {
      return operation.doLong(left, right, index, problemAggregator);
    }
  }
}
