package org.enso.table.aggregations;

import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing the mean value in a group. */
public class Mean extends KnownTypeAggregator {
  private final ColumnStorage<?> storage;
  private final String columnName;

  public Mean(String name, Column column) {
    super(name, resultTypeFromInput(column.getStorage()));
    this.storage = column.getStorage();
    this.columnName = column.getName();
  }

  private static StorageType<?> resultTypeFromInput(ColumnStorage<?> inputStorage) {
    var resolvedStorage = ColumnStorageWithInferredStorage.resolveStorage(inputStorage);
    var inputType = resolvedStorage.getType();
    return resultTypeFromInputType(inputType);
  }

  private static StorageType<?> resultTypeFromInputType(StorageType<?> inputType)
      throws IllegalStateException {
    return switch (inputType) {
      case FloatType floatType -> FloatType.FLOAT_64;
      case IntegerType integerType -> FloatType.FLOAT_64;
      case BigIntegerType bigIntegerType -> BigDecimalType.INSTANCE;
      case BigDecimalType bigDecimalType -> BigDecimalType.INSTANCE;
      case NullType nullType -> nullType;
      default -> {
        if (Proxy.isProxyClass(inputType.getClass())) {
          var fresh = StorageType.fromTypeCharAndSize(inputType.typeChar(), inputType.size());
          yield resultTypeFromInputType(fresh);
        }
        throw new IllegalStateException("Unexpected input type for Mean aggregate: " + inputType);
      }
    };
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    MeanAccumulator accumulator = makeAccumulator();
    accumulator.accumulate(indexes, storage, innerAggregator);
    return accumulator.summarize();
  }

  private MeanAccumulator makeAccumulator() {
    return switch (getType()) {
      case FloatType floatType -> new FloatMeanAccumulator();
      case BigDecimalType bigDecimalType -> new BigDecimalMeanAccumulator();
      case NullType nullType -> new NullAccumulator();
      default ->
          throw new IllegalStateException("Unexpected output type in Mean aggregate: " + getType());
    };
  }

  private abstract static class MeanAccumulator {
    abstract void accumulate(
        List<Integer> indexes, ColumnStorage<?> storage, ProblemAggregator problemAggregator);

    abstract Object summarize();
  }

  private final class FloatMeanAccumulator extends MeanAccumulator {
    private double total = 0;
    private long count = 0;

    @Override
    void accumulate(
        List<Integer> indexes, ColumnStorage<?> storage, ProblemAggregator problemAggregator) {
      Context context = Context.getCurrent();
      if (storage instanceof ColumnDoubleStorage doubleStorage) {
        for (int i : indexes) {
          if (!doubleStorage.isNothing(i)) {
            total += doubleStorage.getItemAsDouble(i);
            count++;
          }
          context.safepoint();
        }
      } else if (storage instanceof ColumnLongStorage longStorage) {
        for (int i : indexes) {
          if (!longStorage.isNothing(i)) {
            total += longStorage.getItemAsLong(i);
            count++;
          }
          context.safepoint();
        }
      } else {
        ColumnAggregatedProblemAggregator innerAggregator =
            new ColumnAggregatedProblemAggregator(problemAggregator);
        for (int i : indexes) {
          Object value = storage.getItemBoxed(i);
          if (value != null) {
            Double dValue = NumericConverter.tryConvertingToDouble(value);
            if (dValue == null) {
              innerAggregator.reportColumnAggregatedProblem(
                  new InvalidAggregation(columnName, i, "Cannot convert to a Float."));
              continue;
            }

            total += dValue;
            count++;
          }
          context.safepoint();
        }
      }
    }

    @Override
    Object summarize() {
      return count == 0 ? null : total / count;
    }
  }

  private final class BigDecimalMeanAccumulator extends MeanAccumulator {
    private BigDecimal total = BigDecimal.ZERO;
    private long count = 0;

    @Override
    void accumulate(
        List<Integer> indexes, ColumnStorage<?> storage, ProblemAggregator problemAggregator) {
      ColumnAggregatedProblemAggregator innerAggregator =
          new ColumnAggregatedProblemAggregator(problemAggregator);
      Context context = Context.getCurrent();
      for (int i : indexes) {
        Object value = storage.getItemBoxed(i);
        if (value != null) {
          try {
            BigDecimal valueAsBigDecimal = NumericConverter.coerceToBigDecimal(value);
            total = total.add(valueAsBigDecimal);
            count++;
          } catch (UnsupportedOperationException error) {
            innerAggregator.reportColumnAggregatedProblem(
                new InvalidAggregation(
                    columnName, i, "Cannot convert to a BigDecimal: " + error.getMessage()));
            continue;
          }
        }
        context.safepoint();
      }
    }

    @Override
    Object summarize() {
      return count == 0 ? null : total.divide(BigDecimal.valueOf(count), MathContext.DECIMAL128);
    }
  }

  /** A special case for a null input column. */
  private static final class NullAccumulator extends MeanAccumulator {

    @Override
    void accumulate(
        List<Integer> indexes, ColumnStorage<?> storage, ProblemAggregator problemAggregator) {
      assert storage.getType() instanceof NullType;
    }

    @Override
    Object summarize() {
      return null;
    }
  }
}
