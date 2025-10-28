package org.enso.table.data.column;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.enso.base.Text_Utils;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.JsonOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.util.LeastRecentlyUsedCache;

public abstract class DataQualityMetrics {
  // A thread pool for executing data quality metrics computations asynchronously.
  private static ExecutorService _threadFactory;

  private static ExecutorService threadFactory() {
    if (_threadFactory == null) {
      _threadFactory =
          Executors.newFixedThreadPool(Math.min(4, Runtime.getRuntime().availableProcessors() / 2));
    }
    return _threadFactory;
  }

  public static final String IS_INCOMPLETE = "_Is Incomplete";
  public static final String NOTHING_COUNT = "# Nothing";
  public static final String DISTINCT_COUNT = "# Distinct";
  public static final String DISTINCT_JSON = "_Distinct JSON";
  public static final String SINGLE_VALUE = "_Single Value";
  public static final String MINIMUM = "Minimum";
  public static final String MAXIMUM = "Maximum";
  public static final String SAMPLED = "_Sampled";
  public static final String EMPTY_COUNT = "@ Empty";
  public static final String UNTRIMMED_COUNT = "@ Untrimmed";
  public static final String ODD_SPACE_COUNT = "@ Non-Trivial Whitespace";
  public static final String NEEDS_FORMATTING = "_Needs Formatting";
  public static final String TYPE_RECORD = "Types and Counts";

  // Default threshold for checking distinct values count.
  public static final int DISTINCT_THRESHOLD = 100;

  // Default seed for random number generation (no specific reason for this value, just stability on
  // results).
  public static final long RANDOM_SEED = 677280131;

  // Default sample size for counting untrimmed cells.
  public static final long DEFAULT_SAMPLE_SIZE = 10000;

  private static Map<Long, DataQualityMetrics> _cachedMetrics;

  private static Map<Long, DataQualityMetrics> cachedMetrics() {
    if (_cachedMetrics == null) {
      _cachedMetrics = new LeastRecentlyUsedCache<>(1000);
    }
    return _cachedMetrics;
  }

  /**
   * Triggers the computation of data quality metrics for the given table. This method is a no-op if
   * the metrics have already been computed.
   *
   * @param table the table to trigger metrics for
   */
  public static void triggerTable(Table table) {
    for (var column : table.getColumns()) {
      DataQualityMetrics.triggerColumn(column);
    }
  }

  /**
   * Triggers the computation of data quality metrics for the given column. This method is a no-op
   * if the metrics have already been computed.
   *
   * @param column the column to trigger metrics for
   */
  public static void triggerColumn(Column column) {
    var storage = column.getStorage();
    get(storage);
  }

  /**
   * Returns the data quality metrics for the given column.
   *
   * @param column the column to get metrics for
   * @return a map of metrics
   */
  public static Map<String, Object> get(Column column) {
    return get(column.getStorage()).getMetrics();
  }

  /**
   * Awaits the completion of any asynchronous computations and returns the metrics for the given
   *
   * @param column the column to get metrics for
   * @return a map of metrics
   */
  public static Map<String, Object> join(Column column) {
    var metrics = get(column.getStorage());
    metrics.join();
    return metrics.getMetrics();
  }

  /**
   * Returns a DataQualityMetrics instance for the given column storage.
   *
   * @param columnStorage the column storage to get metrics for
   * @return a DataQualityMetrics instance
   */
  public static DataQualityMetrics get(ColumnStorage<?> columnStorage) {
    return cachedMetrics()
        .computeIfAbsent(
            columnStorage.uniqueKey(), k -> DataQualityMetrics.createMetrics(columnStorage));
  }

  private static DataQualityMetrics createMetrics(ColumnStorage<?> columnStorage) {
    var resolvedStorage = ColumnStorageWithInferredStorage.resolveStorage(columnStorage);
    return switch (resolvedStorage.getType()) {
      case NullType nullType -> new NullQualityMetrics(resolvedStorage);
      case TextType textType -> new StringQualityMetrics(textType.asTypedStorage(resolvedStorage));
      case FloatType floatType ->
          NumericQualityMetrics.forDouble(floatType.asTypedStorage(resolvedStorage));
      case IntegerType integerType ->
          NumericQualityMetrics.forLong(integerType.asTypedStorage(resolvedStorage));
      case BigIntegerType bigIntegerType ->
          NumericQualityMetrics.forBigInteger(bigIntegerType.asTypedStorage(resolvedStorage));
      case BigDecimalType bigDecimalType ->
          NumericQualityMetrics.forBigDecimal(bigDecimalType.asTypedStorage(resolvedStorage));
      case DateType dateType ->
          new MinMaxQualityMetrics<>(
              dateType.asTypedStorage(resolvedStorage), LocalDate::compareTo);
      case TimeOfDayType timeType ->
          new MinMaxQualityMetrics<>(
              timeType.asTypedStorage(resolvedStorage), LocalTime::compareTo);
      case DateTimeType dateTimeType ->
          new MinMaxQualityMetrics<>(
              dateTimeType.asTypedStorage(resolvedStorage), ZonedDateTime::compareTo);
      case AnyObjectType anyObjectType ->
          new AnyObjectQualityMetric(anyObjectType.asTypedStorage(resolvedStorage));
      default -> new BaseQualityMetrics(resolvedStorage);
    };
  }

  public Map<String, Object> getMetrics() {
    return new HashMap<>();
  }

  protected void join() {
    // This method is a no-op by default, but can be overridden by subclasses to wait for
    // asynchronous computations to complete.
  }

  private static class NullQualityMetrics extends DataQualityMetrics {
    private final long nothingCount;

    public NullQualityMetrics(ColumnStorage<?> columnStorage) {
      nothingCount = columnStorage.getSize();
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();
      current.put(NOTHING_COUNT, nothingCount);
      current.put(DISTINCT_COUNT, 0L);
      return current;
    }
  }

  private static class BaseQualityMetrics extends DataQualityMetrics {
    private static class Accumulator {
      private long nothingCount = 0;
      private final Set<Object> distinct = new HashSet<>();

      public void process(Object value) {
        if (value == null) {
          nothingCount += 1;
        } else {
          distinct.add(value);
        }
      }

      public Result getResult() {
        String distinctJson = null;
        if (distinct.size() < DISTINCT_THRESHOLD) {
          distinctJson =
              "["
                  + distinct.stream()
                      .map(v -> JsonOperation.objectToJson(v, o -> null))
                      .filter(Objects::nonNull)
                      .sorted()
                      .collect(Collectors.joining())
                  + "]";
        }
        return new Result(nothingCount, distinct.size(), distinctJson);
      }
    }

    private record Result(long nothingCount, long distinctCount, String distinctJson) {}

    private final CompletableFuture<Result> result;

    public BaseQualityMetrics(ColumnStorage<?> storage) {
      if (storage.getType() instanceof NullType) {
        result = CompletableFuture.completedFuture(new Result(0, 0, ""));
      } else {
        result =
            CompletableFuture.supplyAsync(
                () -> {
                  Accumulator accumulator = new Accumulator();
                  DataQualityMetrics.loopOverAll(storage, accumulator::process);
                  return accumulator.getResult();
                },
                threadFactory());
      }
    }

    @Override
    protected void join() {
      result.join();
      super.join();
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null) {
        current.put(NOTHING_COUNT, currentResult.nothingCount);
        current.put(DISTINCT_COUNT, currentResult.distinctCount);
        if (currentResult.distinctJson != null) {
          current.put(DISTINCT_JSON, currentResult.distinctJson);
        }
      } else if (!result.isDone()) {
        current.put(IS_INCOMPLETE, true);
      }

      return current;
    }
  }

  private static class MinMaxQualityMetrics<T> extends BaseQualityMetrics {
    private static class Accumulator<T> {
      private final Comparator<T> comparator;
      private T minimum = null;
      private T maximum = null;

      public Accumulator(Comparator<T> comparator) {
        this.comparator = comparator;
      }

      public void process(T value) {
        if (value == null) {
          return;
        }
        if (minimum == null || comparator.compare(value, minimum) < 0) {
          minimum = value;
        }
        if (maximum == null || comparator.compare(value, maximum) > 0) {
          maximum = value;
        }
      }

      public Result<T> getResult() {
        return new Result<>(minimum, maximum);
      }
    }

    private record Result<T>(T minimum, T maximum) {}

    private final CompletableFuture<Result<T>> result;

    public MinMaxQualityMetrics(ColumnStorage<T> storage, Comparator<T> comparator) {
      super(storage);
      result =
          CompletableFuture.supplyAsync(
              () -> {
                Accumulator<T> accumulator = new Accumulator<>(comparator);
                DataQualityMetrics.loopOverAll(storage, accumulator::process);
                return accumulator.getResult();
              },
              threadFactory());
    }

    @Override
    protected void join() {
      result.join();
      super.join();
    }

    public T getMinimum() {
      return result.thenApply(Result::minimum).getNow(null);
    }

    public T getMaximum() {
      return result.thenApply(Result::maximum).getNow(null);
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null && currentResult.minimum != null) {
        current.put(SINGLE_VALUE, currentResult.minimum.equals(currentResult.maximum));
        current.put(MINIMUM, currentResult.minimum);
        current.put(MAXIMUM, currentResult.maximum);
      } else if (!result.isDone()) {
        current.put(IS_INCOMPLETE, true);
      }

      return current;
    }
  }

  private static class StringQualityMetrics extends MinMaxQualityMetrics<String> {
    private static class Accumulator {
      private long emptyCount = 0;
      private long untrimmedCount = 0;
      private long nonTrivialWhitespaceCount = 0;

      public void process(String value) {
        if (value == null) {
          return;
        }

        if (value.isEmpty()) {
          emptyCount += 1;
        } else {
          if (Text_Utils.has_leading_trailing_whitespace(value)) {
            untrimmedCount += 1;
          }
          if (Text_Utils.has_non_trivial_whitespace(value)) {
            nonTrivialWhitespaceCount += 1;
          }
        }
      }

      public Result getResult(boolean sampled) {
        return new Result(sampled, emptyCount, untrimmedCount, nonTrivialWhitespaceCount);
      }
    }

    private record Result(boolean sampled, long empty, long untrimmed, long notTrivialWhitespace) {}

    private final CompletableFuture<Result> result;

    public StringQualityMetrics(ColumnStorage<String> storage) {
      super(storage, String::compareTo);
      result =
          CompletableFuture.supplyAsync(
              () -> {
                var accumulator = new Accumulator();
                DataQualityMetrics.loopOverSample(storage, accumulator::process);
                return accumulator.getResult(storage.getSize() > DEFAULT_SAMPLE_SIZE);
              },
              threadFactory());
    }

    @Override
    protected void join() {
      result.join();
      super.join();
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null) {
        if (currentResult.sampled) {
          current.put(SAMPLED, currentResult.sampled);
        }
        current.put(EMPTY_COUNT, currentResult.empty);
        current.put(UNTRIMMED_COUNT, currentResult.untrimmed);
        current.put(ODD_SPACE_COUNT, currentResult.notTrivialWhitespace);
      } else if (!result.isDone()) {
        current.put(IS_INCOMPLETE, true);
      }

      return current;
    }
  }

  private static class NumericQualityMetrics<T> extends MinMaxQualityMetrics<T> {
    private static final long FORMAT_NUMBER_LIMIT = 999999;

    public static NumericQualityMetrics<Double> forDouble(ColumnStorage<Double> storage) {
      return new NumericQualityMetrics<>(
          storage, Double::compareTo, (double) -FORMAT_NUMBER_LIMIT, (double) FORMAT_NUMBER_LIMIT);
    }

    public static NumericQualityMetrics<Long> forLong(ColumnStorage<Long> storage) {
      return new NumericQualityMetrics<>(
          storage, Long::compareTo, -FORMAT_NUMBER_LIMIT, FORMAT_NUMBER_LIMIT);
    }

    public static NumericQualityMetrics<BigInteger> forBigInteger(
        ColumnStorage<BigInteger> storage) {
      return new NumericQualityMetrics<>(
          storage,
          BigInteger::compareTo,
          BigInteger.valueOf(-FORMAT_NUMBER_LIMIT),
          BigInteger.valueOf(FORMAT_NUMBER_LIMIT));
    }

    public static NumericQualityMetrics<BigDecimal> forBigDecimal(
        ColumnStorage<BigDecimal> storage) {
      return new NumericQualityMetrics<>(
          storage,
          BigDecimal::compareTo,
          BigDecimal.valueOf(-FORMAT_NUMBER_LIMIT),
          BigDecimal.valueOf(FORMAT_NUMBER_LIMIT));
    }

    private final Comparator<T> comparator;
    private final T minLimit;
    private final T maxLimit;

    private NumericQualityMetrics(
        ColumnStorage<T> storage, Comparator<T> comparator, T minLimit, T maxLimit) {
      super(storage, comparator);
      this.comparator = comparator;
      this.minLimit = minLimit;
      this.maxLimit = maxLimit;
    }

    public Boolean getNeedsFormatting() {
      var minimum = getMinimum();
      if (minimum == null) {
        return null;
      }

      var maximum = getMaximum();
      if (maximum == null) {
        return null;
      }

      return (comparator.compare(minimum, minLimit) < 0)
          || (comparator.compare(maximum, maxLimit) > 0);
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = getNeedsFormatting();
      if (currentResult != null) {
        current.put(NEEDS_FORMATTING, currentResult);
      }

      return current;
    }
  }

  private static class AnyObjectQualityMetric extends BaseQualityMetrics {
    private static class Accumulator {
      private final Map<String, Long> typeCounts = new HashMap<>();

      public void process(Object value) {
        if (value == null) {
          return;
        }

        if (value instanceof BigDecimal) {
          typeCounts.merge("Decimal", 1L, Long::sum);
        } else if (NumericConverter.isCoercibleToBigInteger(value)) {
          typeCounts.merge("Integer", 1L, Long::sum);
        } else if (NumericConverter.isCoercibleToDouble(value)) {
          typeCounts.merge("Float", 1L, Long::sum);
        } else if (value instanceof LocalDate) {
          typeCounts.merge("Date", 1L, Long::sum);
        } else if (value instanceof LocalTime) {
          typeCounts.merge("Time", 1L, Long::sum);
        } else if (value instanceof ZonedDateTime) {
          typeCounts.merge("Date_Time", 1L, Long::sum);
        } else if (value instanceof String) {
          typeCounts.merge("Char", 1L, Long::sum);
        } else if (value instanceof Boolean) {
          typeCounts.merge("Boolean", 1L, Long::sum);
        } else {
          typeCounts.merge("Other", 1L, Long::sum);
        }
      }

      public Result getResult() {
        String typeRecord =
            typeCounts.entrySet().stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .map(entry -> entry.getKey() + ": " + entry.getValue())
                .reduce((a, b) -> a + "\n" + b)
                .orElse(null);
        return new Result(typeRecord);
      }
    }

    private record Result(String typeRecord) {}

    private final CompletableFuture<Result> result;

    public AnyObjectQualityMetric(ColumnStorage<Object> storage) {
      super(storage);
      result =
          CompletableFuture.supplyAsync(
              () -> {
                Accumulator accumulator = new Accumulator();
                DataQualityMetrics.loopOverAll(storage, accumulator::process);
                return accumulator.getResult();
              },
              threadFactory());
    }

    @Override
    protected void join() {
      result.join();
      super.join();
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null && currentResult.typeRecord != null) {
        current.put(TYPE_RECORD, currentResult.typeRecord);
      } else if (!result.isDone()) {
        current.put(IS_INCOMPLETE, true);
      }

      return current;
    }
  }

  private static <S> void loopOverAll(ColumnStorage<S> storage, Consumer<S> consumer) {
    long size = storage.getSize();
    for (long idx = 0; idx < size; idx++) {
      // Generate a random index to sample from the storage.
      consumer.accept(storage.getItemBoxed(idx));
    }
  }

  private static <S> void loopOverSample(ColumnStorage<S> storage, Consumer<S> consumer) {
    var rng = new Random(RANDOM_SEED);
    long size = storage.getSize();
    if (size < DEFAULT_SAMPLE_SIZE) {
      // If the size is smaller than the sample size, we just loop over all items.
      loopOverAll(storage, consumer);
      return;
    }

    for (long i = 0; i < DEFAULT_SAMPLE_SIZE; i++) {
      // Generate a random index to sample from the storage.
      long idx = rng.nextLong(size);
      consumer.accept(storage.getItemBoxed(idx));
    }
  }
}
