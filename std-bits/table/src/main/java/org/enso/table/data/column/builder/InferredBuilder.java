package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.problems.ProblemAggregator;

/**
 * A builder performing type inference on the appended elements, choosing the best possible storage.
 */
public class InferredBuilder extends Builder {
  private TypedBuilder currentBuilder = null;
  private int currentSize = 0;
  private final int initialSize;
  private final ProblemAggregator problemAggregator;
  private final boolean allowDateToDateTimeConversion;

  /**
   * Creates a new instance of this builder, with the given known result length.
   *
   * @param initialSize the result length
   * @param problemAggregator the problem aggregator to use
   */
  public InferredBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this(initialSize, problemAggregator, false);
  }

  /**
   * Creates a new instance of this builder, with the given known result length. This is a special
   * constructor that allows for date to date-time conversion (for Excel).
   *
   * @param initialSize the result length
   * @param problemAggregator the problem aggregator to use
   * @param allowDateToDateTimeConversion whether to allow date to date-time conversion
   */
  public InferredBuilder(
      int initialSize, ProblemAggregator problemAggregator, boolean allowDateToDateTimeConversion) {
    this.initialSize = initialSize;
    this.problemAggregator = problemAggregator;
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (currentBuilder == null) {
      if (o == null) {
        currentSize++;
        return;
      } else {
        initBuilderFor(o);
      }
    }
    if (o == null) {
      currentBuilder.appendNoGrow(o);
    } else {
      if (currentBuilder.accepts(o)) {
        currentBuilder.appendNoGrow(o);
      } else {
        retypeAndAppend(o);
      }
    }
    currentSize++;
  }

  @Override
  public void append(Object o) {
    // ToDo: This a workaround for an issue with polyglot layer. #5590 is related.
    o = Polyglot_Utils.convertPolyglotValue(o);

    if (currentBuilder == null) {
      if (o == null) {
        currentSize++;
        return;
      } else {
        initBuilderFor(o);
      }
    }
    if (o == null) {
      currentBuilder.append(o);
    } else {
      if (currentBuilder.accepts(o)) {
        currentBuilder.append(o);
      } else {
        retypeAndAppend(o);
      }
    }
    currentSize++;
  }

  @Override
  public void appendNulls(int count) {
    if (currentBuilder != null) {
      currentBuilder.appendNulls(count);
    }
    currentSize += count;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    for (int i = 0; i < storage.size(); i++) {
      append(storage.getItemBoxed(i));
    }
  }

  private void initBuilderFor(Object o) {
    int initialCapacity = Math.max(initialSize, currentSize);
    if (o instanceof Boolean) {
      currentBuilder = new BoolBuilder();
    } else if (NumericConverter.isCoercibleToLong(o)) {
      // In inferred builder, we always default to 64-bits.
      currentBuilder =
          NumericBuilder.createLongBuilder(initialCapacity, IntegerType.INT_64, problemAggregator);
    } else if (NumericConverter.isFloatLike(o)) {
      currentBuilder =
          NumericBuilder.createInferringDoubleBuilder(initialCapacity, problemAggregator);
    } else if (o instanceof String) {
      currentBuilder = new StringBuilder(initialCapacity, TextType.VARIABLE_LENGTH);
    } else if (o instanceof BigInteger) {
      currentBuilder = new BigIntegerBuilder(initialCapacity, problemAggregator);
    } else if (o instanceof BigDecimal) {
      currentBuilder = new BigDecimalBuilder(initialCapacity);
    } else if (o instanceof LocalDate) {
      currentBuilder = new DateBuilder(initialCapacity, allowDateToDateTimeConversion);
    } else if (o instanceof LocalTime) {
      currentBuilder = new TimeOfDayBuilder(initialCapacity);
    } else if (o instanceof ZonedDateTime) {
      currentBuilder = new DateTimeBuilder(initialCapacity, allowDateToDateTimeConversion);
    } else {
      currentBuilder = new MixedBuilder(initialCapacity);
    }
    currentBuilder.appendNulls(currentSize);
  }

  private record RetypeInfo(Class<?> clazz, StorageType type) {}

  private static final List<RetypeInfo> retypePairs =
      List.of(
          new RetypeInfo(Boolean.class, BooleanType.INSTANCE),
          new RetypeInfo(Long.class, IntegerType.INT_64),
          new RetypeInfo(Double.class, FloatType.FLOAT_64),
          new RetypeInfo(String.class, TextType.VARIABLE_LENGTH),
          new RetypeInfo(BigDecimal.class, BigDecimalType.INSTANCE),
          new RetypeInfo(LocalDate.class, DateType.INSTANCE),
          new RetypeInfo(LocalTime.class, TimeOfDayType.INSTANCE),
          new RetypeInfo(ZonedDateTime.class, DateTimeType.INSTANCE),
          new RetypeInfo(Float.class, FloatType.FLOAT_64),
          // Smaller integer types are upcast to 64-bit integers by default anyway. This logic does
          // not apply only if a specific type is requested (so not in inferred builder).
          new RetypeInfo(Integer.class, IntegerType.INT_64),
          new RetypeInfo(Short.class, IntegerType.INT_64),
          new RetypeInfo(Byte.class, IntegerType.INT_64),
          new RetypeInfo(BigInteger.class, BigIntegerType.INSTANCE),
          // Will only return true if the date to date-time conversion is allowed.
          new RetypeInfo(LocalDate.class, DateTimeType.INSTANCE));

  private void retypeAndAppend(Object o) {
    for (RetypeInfo info : retypePairs) {
      if (info.clazz.isInstance(o) && currentBuilder.canRetypeTo(info.type)) {
        currentBuilder = currentBuilder.retypeTo(info.type);
        currentBuilder.append(o);
        return;
      }
    }

    retypeToMixed();
    currentBuilder.append(o);
  }

  private void retypeToMixed() {
    // The new internal builder must be at least `currentSize` so it can store
    // all the current values. It must also be at least 'initialSize' since the
    // caller might be using appendNoGrow and is expecting to write at least
    // that many values.
    int capacity = Math.max(initialSize, currentSize);

    ObjectBuilder objectBuilder = new MixedBuilder(capacity);
    currentBuilder.retypeToMixed(objectBuilder.getData());
    objectBuilder.setCurrentSize(currentBuilder.getCurrentSize());
    currentBuilder = objectBuilder;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage<?> seal() {
    if (currentBuilder == null) {
      initBuilderFor(null);
    }
    return currentBuilder.seal();
  }

  @Override
  public StorageType getType() {
    // The type of InferredBuilder can change over time, so we do not report any stable type here.
    return null;
  }
}
