package org.enso.table.data.column.storage.type;

import java.time.LocalTime;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

public final class TimeOfDayType implements StorageType<LocalTime> {
  public static final TimeOfDayType INSTANCE = new TimeOfDayType();

  private TimeOfDayType() {}

  @Override
  public char typeChar() {
    return 'W';
  }

  @Override
  public Value asEnsoValueType() {
    return EnsoMeta.makeInstance("Standard.Table.Value_Type", "Value_Type", "Time");
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
    if (StorageType.ofStorage(storage) instanceof TimeOfDayType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<LocalTime>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of TimeOfDayType");
  }
}
