package org.enso.table.data.column;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.function.Predicate;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.NumericType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.util.LeastRecentlyUsedCache;

public abstract class DataQualityMetrics {
  // Default seed for random number generation (no specific reason for this value, just stability on
  // results).
  public static final long RANDOM_SEED = 677280131;

  // Default sample size for counting untrimmed cells.
  public static final long DEFAULT_SAMPLE_SIZE = 10000;

  private static Map<ColumnStorage<?>, DataQualityMetrics> _cachedMetrics;

  private static Map<ColumnStorage<?>, DataQualityMetrics> cachedMetrics() {
    if (_cachedMetrics == null) {
      _cachedMetrics = new LeastRecentlyUsedCache<>(1000);
    }
    return _cachedMetrics;
  }

  public static Map<String, Object> get(Column column) {
    return getMetrics(column.getStorage()).getMetrics();
  }

  public static DataQualityMetrics getMetrics(ColumnStorage<?> columnStorage) {
    return cachedMetrics().computeIfAbsent(columnStorage, DataQualityMetrics::createMetrics);
  }

  private static DataQualityMetrics createMetrics(ColumnStorage<?> columnStorage) {
    return switch (columnStorage.getType()) {
      case NullType nullType -> new NullQualityMetrics(columnStorage);
      case TextType textType -> new StringQualityMetrics(textType.asTypedStorage(columnStorage));
      case AnyObjectType anyObjectType -> new StringQualityMetrics(
          anyObjectType.asTypedStorage(columnStorage), true);
      case NumericType numericType -> new NumericQualityMetrics(columnStorage);
      default -> new BaseQualityMetrics(columnStorage);
    };
  }

  public Map<String, Object> getMetrics() {
    return new HashMap<>();
  }

  private static class NullQualityMetrics extends DataQualityMetrics {
    private final long nothingCount;

    public NullQualityMetrics(ColumnStorage<?> columnStorage) {
      nothingCount = columnStorage.getSize();
    }

    public Long getNothingCount() {
      return nothingCount;
    }

    public Long getDistinctCount() {
      return 0L;
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();
      current.put("nothingCount", getNothingCount());
      current.put("distinctCount", getDistinctCount());
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
        return new Result(nothingCount, distinct.size());
      }
    }

    private record Result(long nothingCount, long distinctCount) {}

    private final CompletableFuture<Result> result;

    public BaseQualityMetrics(ColumnStorage<?> storage) {
      if (storage.getType() instanceof NullType) {
        this.result = CompletableFuture.completedFuture(new Result(0, 0));
      } else {
        this.result =
            CompletableFuture.supplyAsync(
                () -> {
                  Accumulator accumulator = new Accumulator();
                  DataQualityMetrics.loopOverAll(storage, accumulator::process);
                  return accumulator.getResult();
                });
      }
    }

    public Long getNothingCount() {
      var current = result.getNow(null);
      return current != null ? current.nothingCount : null;
    }

    public Long getDistinctCount() {
      var current = result.getNow(null);
      return current != null ? current.distinctCount : null;
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null) {
        current.put("nothingCount", currentResult.nothingCount);
        current.put("distinctCount", currentResult.distinctCount);
      }

      return current;
    }
  }

  private static class StringQualityMetrics extends BaseQualityMetrics {
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
      super(storage);
      this.result =
          CompletableFuture.supplyAsync(
              () -> {
                var accumulator = new Accumulator();
                DataQualityMetrics.loopOverSample(storage, accumulator::process);
                return accumulator.getResult(storage.getSize() <= DEFAULT_SAMPLE_SIZE);
              });
    }

    public StringQualityMetrics(ColumnStorage<Object> storage, boolean anyObject) {
      super(storage);
      this.result =
          CompletableFuture.supplyAsync(
              () -> {
                var accumulator = new Accumulator();
                DataQualityMetrics.loopOverSample(
                    storage,
                    value -> {
                      if (value instanceof String str) {
                        accumulator.process(str);
                      }
                    });
                return accumulator.getResult(storage.getSize() <= DEFAULT_SAMPLE_SIZE);
              });
    }

    public Boolean getSampled() {
      var current = result.getNow(null);
      return current != null ? current.sampled : null;
    }

    public Long getEmptyCount() {
      var current = result.getNow(null);
      return current != null ? current.empty : null;
    }

    public Long getUntrimmedCount() {
      var current = result.getNow(null);
      return current != null ? current.untrimmed : null;
    }

    public Long getNonTrivialWhitespaceCount() {
      var current = result.getNow(null);
      return current != null ? current.notTrivialWhitespace : null;
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = result.getNow(null);
      if (currentResult != null) {
        current.put("sampled", currentResult.sampled);
        current.put("emptyCount", currentResult.empty);
        current.put("untrimmedCount", currentResult.untrimmed);
        current.put("notTrivialWhitespaceCount", currentResult.notTrivialWhitespace);
      }

      return current;
    }
  }

  private static class NumericQualityMetrics extends BaseQualityMetrics {
    private static final int FORMAT_NUMBER_LIMIT = 999999;

    private static class Accumulator {
      private boolean needsFormatting = false;

      public boolean process(Object value) {
        needsFormatting =
            switch (value) {
              case Long longValue -> longValue < -FORMAT_NUMBER_LIMIT
                  || longValue > FORMAT_NUMBER_LIMIT;
              case Double doubleValue -> doubleValue < -FORMAT_NUMBER_LIMIT
                  || doubleValue > FORMAT_NUMBER_LIMIT;
              case BigInteger bigIntegerValue -> bigIntegerValue.compareTo(
                          BigInteger.valueOf(-FORMAT_NUMBER_LIMIT))
                      < 0
                  || bigIntegerValue.compareTo(BigInteger.valueOf(FORMAT_NUMBER_LIMIT)) > 0;
              case BigDecimal bigDecimalValue -> bigDecimalValue.compareTo(
                          BigDecimal.valueOf(-FORMAT_NUMBER_LIMIT))
                      < 0
                  || bigDecimalValue.compareTo(BigDecimal.valueOf(FORMAT_NUMBER_LIMIT)) > 0;
              default -> false;
            };

        return needsFormatting;
      }

      public boolean getNeedsFormatting() {
        return needsFormatting;
      }
    }

    private final CompletableFuture<Boolean> needsFormatting;

    public NumericQualityMetrics(ColumnStorage<?> storage) {
      super(storage);
      this.needsFormatting =
          CompletableFuture.supplyAsync(
              () -> {
                var accumulator = new Accumulator();
                DataQualityMetrics.loopOverAll(storage, accumulator::process);
                return accumulator.getNeedsFormatting();
              });
    }

    public Boolean getNeedsFormatting() {
      return needsFormatting.getNow(null);
    }

    @Override
    public Map<String, Object> getMetrics() {
      var current = super.getMetrics();

      var currentResult = needsFormatting.getNow(null);
      if (currentResult != null) {
        current.put("needsFormatting", currentResult);
      }

      return current;
    }
  }

  private static <S> void loopOverAll(ColumnStorage<S> storage, Predicate<S> predicate) {
    long size = storage.getSize();
    for (long idx = 0; idx < size; idx++) {
      // Generate a random index to sample from the storage.
      if (predicate.test(storage.getItemBoxed(idx))) {
        return;
      }
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
      long idx = rng.nextInt(Math.toIntExact(size));
      consumer.accept(storage.getItemBoxed(idx));
    }
  }
}
