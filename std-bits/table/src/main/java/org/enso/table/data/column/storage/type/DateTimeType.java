package org.enso.table.data.column.storage.type;

import java.time.ZonedDateTime;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class DateTimeType implements StorageType<ZonedDateTime> {
  public static final DateTimeType INSTANCE = new DateTimeType(true);
  public static final DateTimeType INSTANCE_NO_TZ = new DateTimeType(false);

  private final boolean hasTimeZone;

  private DateTimeType(boolean hasTimeZone) {
    this.hasTimeZone = hasTimeZone;
  }

  @Override
  public char typeChar() {
    return hasTimeZone ? 'Z' : 'Y';
  }

  /**
   * Returns true if the DateTimeType includes timezone information.
   *
   * @return true if the DateTimeType includes timezone information.
   */
  public boolean hasTimeZone() {
    return hasTimeZone;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return true;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof DateTimeType;
  }

  @Override
  public ZonedDateTime valueAsType(Object value) {
    value = Polyglot_Utils.convertPolyglotValue(value);
    return value instanceof ZonedDateTime zonedDateTime ? zonedDateTime : null;
  }

  @Override
  public BuilderForType<ZonedDateTime> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForDateTime(initialCapacity);
  }

  @Override
  public ColumnStorage<ZonedDateTime> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateTimeType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<ZonedDateTime>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of DateTimeType");
  }
}
