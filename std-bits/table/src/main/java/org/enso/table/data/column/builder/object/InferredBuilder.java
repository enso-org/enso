package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.List;

/**
 * A builder performing type inference on the appended elements, choosing the best possible storage.
 */
public class InferredBuilder extends Builder {
  private TypedBuilder currentBuilder = null;
  private int currentSize = 0;
  private final int initialSize;

  /**
   * Creates a new instance of this builder, with the given known result size.
   *
   * @param initialSize the result size
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

  private void initBuilderFor(Object o) {
    int initialCapacity = Math.max(initialSize, currentSize);
    if (o instanceof Boolean) {
      currentBuilder = new BoolBuilder();
    } else if (o instanceof Double || o instanceof BigDecimal) {
      currentBuilder = NumericBuilder.createDoubleBuilder(initialCapacity);
    } else if (o instanceof Long) {
      currentBuilder = NumericBuilder.createLongBuilder(initialCapacity);
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

  private record RetypeInfo(Class<?> clazz, int type) {}

  private static final List<RetypeInfo> retypePairs =
      List.of(
          new RetypeInfo(Boolean.class, Storage.Type.BOOL),
          new RetypeInfo(Long.class, Storage.Type.LONG),
          new RetypeInfo(Double.class, Storage.Type.DOUBLE),
          new RetypeInfo(BigDecimal.class, Storage.Type.DOUBLE),
          new RetypeInfo(LocalDate.class, Storage.Type.DATE),
          new RetypeInfo(LocalTime.class, Storage.Type.TIME_OF_DAY),
          new RetypeInfo(ZonedDateTime.class, Storage.Type.DATE_TIME),
          new RetypeInfo(String.class, Storage.Type.STRING));

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
  public Storage seal() {
    if (currentBuilder == null) {
      initBuilderFor(null);
    }
    return currentBuilder.seal();
  }
}
