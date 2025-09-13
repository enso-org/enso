package org.enso.table.data.column.storage.type;

import java.time.LocalTime;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class TimeOfDayType implements StorageType<LocalTime> {
  public static final TimeOfDayType INSTANCE = new TimeOfDayType();

  private TimeOfDayType() {}

  @Override
  public char typeChar() {
    return 'W';
  }

  @Override
  public boolean hasTime() {
    return true;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof TimeOfDayType;
  }

  @Override
  public LocalTime valueAsType(Object value) {
    value = Polyglot_Utils.convertPolyglotValue(value);
    return (value instanceof LocalTime time) ? time : null;
  }

  @Override
  public BuilderForType<LocalTime> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForTime(initialCapacity);
  }

  @Override
  public ColumnStorage<LocalTime> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof TimeOfDayType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<LocalTime>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of TimeOfDayType");
  }
}
