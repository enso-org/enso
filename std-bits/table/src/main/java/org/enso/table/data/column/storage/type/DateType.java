package org.enso.table.data.column.storage.type;

import java.time.LocalDate;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class DateType implements StorageType<LocalDate> {
  public static final DateType INSTANCE = new DateType();

  private DateType() {}

  @Override
  public char typeChar() {
    return 'X';
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof DateType;
  }

  @Override
  public LocalDate valueAsType(Object value) {
    value = Polyglot_Utils.convertPolyglotValue(value);
    return value instanceof LocalDate localDate ? localDate : null;
  }

  @Override
  public BuilderForType<LocalDate> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForDate(initialCapacity);
  }

  @Override
  public ColumnStorage<LocalDate> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<LocalDate>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of DateType");
  }
}
