package org.enso.table.data.column.builder;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

/**
 * A builder performing type inference on the appended elements, choosing the best possible storage.
 */
public class InferredBuilder extends Builder {
  private TypedBuilder currentBuilder = null;
  private int currentSize = 0;
  private final int initialSize;

  /**
   * Creates a new instance of this builder, with the given known result length.
   *
   * @param initialSize the result length
   */
  public InferredBuilder(int initialSize) {
    this.initialSize = initialSize;
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
      currentBuilder = NumericBuilder.createLongBuilder(initialCapacity);
    } else if (NumericConverter.isCoercibleToDouble(o)) {
      currentBuilder = NumericBuilder.createDoubleBuilder(initialCapacity);
    } else if (o instanceof LocalDate) {
      currentBuilder = new DateBuilder(initialCapacity);
    } else if (o instanceof LocalTime) {
      currentBuilder = new TimeOfDayBuilder(initialCapacity);
    } else if (o instanceof ZonedDateTime) {
      currentBuilder = new DateTimeBuilder(initialCapacity);
    } else if (o instanceof String) {
      currentBuilder = new StringBuilder(initialCapacity);
    } else {
      currentBuilder = new ObjectBuilder(initialCapacity);
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
          // TODO [RW] I think BigDecimals should not be coerced to floats, we should add Decimal
          // support to in-memory tables at some point
          // new RetypeInfo(BigDecimal.class, StorageType.FLOAT_64),
          new RetypeInfo(LocalDate.class, DateType.INSTANCE),
          new RetypeInfo(LocalTime.class, TimeOfDayType.INSTANCE),
          new RetypeInfo(ZonedDateTime.class, DateTimeType.INSTANCE),
          new RetypeInfo(Float.class, FloatType.FLOAT_64),
          // Smaller integer types are upcast to 64-bit integers by default anyway. This logic does
          // not apply only if a specific type is requested (so not in inferred builder).
          new RetypeInfo(Integer.class, IntegerType.INT_64),
          new RetypeInfo(Short.class, IntegerType.INT_64),
          new RetypeInfo(Byte.class, IntegerType.INT_64));

  private void retypeAndAppend(Object o) {
    for (RetypeInfo info : retypePairs) {
      if (info.clazz.isInstance(o) && currentBuilder.canRetypeTo(info.type)) {
        currentBuilder = currentBuilder.retypeTo(info.type);
        currentBuilder.append(o);
        return;
      }
    }

    retypeToObject();
    currentBuilder.append(o);
  }

  private void retypeToObject() {
    ObjectBuilder objectBuilder = new ObjectBuilder(initialSize);
    currentBuilder.writeTo(objectBuilder.getData());
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
