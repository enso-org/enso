package org.enso.table.aggregations;

import java.math.BigInteger;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredIntegerBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing the total value in a group. */
public class Sum extends Aggregator {
  private final Storage<?> inputStorage;
  private final StorageType inputType;

  public Sum(String name, Column column) {
    super(name);
    this.inputStorage = column.getStorage();
    inputType = inputStorage.inferPreciseType();
  }

  @Override
  public Builder makeBuilder(int size, ProblemAggregator problemAggregator) {
    return switch (inputType) {
      case IntegerType integerType -> new InferredIntegerBuilder(size, problemAggregator);
      case BigIntegerType bigIntegerType -> Builder.getForType(
          bigIntegerType, size, problemAggregator);
      case FloatType floatType -> Builder.getForDouble(floatType, size, problemAggregator);
      default -> throw new IllegalStateException(
          "Unexpected input type for Sum aggregate: " + inputType);
    };
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    MapOperationProblemAggregator innerAggregator =
        new MapOperationProblemAggregator(problemAggregator, getName());
    SumAccumulator accumulator = makeAccumulator();
    accumulator.accumulate(indexes, inputStorage);
    return accumulator.summarize();
  }

  private SumAccumulator makeAccumulator() {
    return switch (inputType) {
      case IntegerType integerType -> new IntegerSumAccumulator();
      case BigIntegerType bigIntegerType -> new IntegerSumAccumulator();
      case FloatType floatType -> new FloatSumAccumulator();
      default -> throw new IllegalStateException(
          "Unexpected input type for Sum aggregate: " + inputType);
    };
  }

  private abstract static class SumAccumulator {
    abstract void accumulate(List<Integer> indexes, Storage<?> storage);

    abstract Object summarize();
  }

  private static final class IntegerSumAccumulator extends SumAccumulator {
    private Object accumulator = null;

    void add(Object value) {
      if (value == null) {
        return;
      }

      Long valueAsLong = NumericConverter.tryConvertingToLong(value);
      if (valueAsLong != null) {
        addLong(valueAsLong);
      } else if (value instanceof BigInteger) {
        addBigInteger((BigInteger) value);
      } else {
        throw new IllegalStateException("Unexpected value type: " + value.getClass());
      }
    }

    @Override
    void accumulate(List<Integer> indexes, Storage<?> storage) {
      Context context = Context.getCurrent();
      if (storage instanceof AbstractLongStorage longStorage) {
        for (int row : indexes) {
          if (!longStorage.isNothing(row)) {
            addLong(longStorage.getItem(row));
          }
          context.safepoint();
        }
      } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
        for (int row : indexes) {
          BigInteger value = bigIntegerStorage.getItem(row);
          if (value != null) {
            addBigInteger(value);
          }
          context.safepoint();
        }
      } else {
        for (int row : indexes) {
          add(storage.getItemBoxed(row));
          context.safepoint();
        }
      }
    }

    private void addLong(long value) {
      switch (accumulator) {
        case Long accumulatorAsLong -> {
          try {
            accumulator = Math.addExact(accumulatorAsLong, value);
          } catch (ArithmeticException exception) {
            accumulator = BigInteger.valueOf(accumulatorAsLong).add(BigInteger.valueOf(value));
          }
        }
        case BigInteger accumulatorAsBigInteger -> {
          accumulator = accumulatorAsBigInteger.add(BigInteger.valueOf(value));
        }
        case null -> {
          accumulator = value;
        }
        default -> throw new IllegalStateException(
            "Unexpected accumulator type: " + accumulator.getClass());
      }
    }

    private void addBigInteger(BigInteger value) {
      assert value != null;
      switch (accumulator) {
        case Long accumulatorAsLong -> accumulator =
            BigInteger.valueOf(accumulatorAsLong).add(value);
        case BigInteger accumulatorAsBigInteger -> accumulator = accumulatorAsBigInteger.add(value);
        case null -> accumulator = value;
        default -> throw new IllegalStateException(
            "Unexpected accumulator type: " + accumulator.getClass());
      }
    }

    Object summarize() {
      return accumulator;
    }
  }

  private static final class FloatSumAccumulator extends SumAccumulator {
    private Double accumulator = null;

    void add(Object value) {
      if (value == null) {
        return;
      }

      Double valueAsDouble = NumericConverter.tryConvertingToDouble(value);
      if (valueAsDouble != null) {
        addDouble(valueAsDouble);
      } else {
        throw new IllegalStateException("Unexpected value type: " + value.getClass());
      }
    }

    @Override
    void accumulate(List<Integer> indexes, Storage<?> storage) {
      Context context = Context.getCurrent();
      if (storage instanceof DoubleStorage doubleStorage) {
        for (int row : indexes) {
          if (!doubleStorage.isNothing(row)) {
            addDouble(doubleStorage.getItem(row));
          }
          context.safepoint();
        }
      } else {
        for (int row : indexes) {
          add(storage.getItemBoxed(row));
          context.safepoint();
        }
      }
    }

    private void addDouble(double value) {
      if (accumulator == null) {
        accumulator = value;
      } else {
        accumulator += value;
      }
    }

    Double summarize() {
      return accumulator;
    }
  }
}
