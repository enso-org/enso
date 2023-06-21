package org.enso.table.data.column.operation.map.datetime;

import java.util.HashSet;
import java.util.List;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.Storage;

/**
 * An IS_IN implementation which ensures the Enso Date/Time types are correctly coerced.
 *
 * <p>It uses the provided {@code storageClass} to only keep the elements that are of the same type
 * as expected in the storage.
 */
public class DateTimeIsInOp<T, S extends Storage<T>> extends SpecializedIsInOp<T, S> {
  private final Class<T> storedType;

  public DateTimeIsInOp(Class<T> storedType) {
    this.storedType = storedType;
  }

  @Override
  protected CompactRepresentation<T> prepareList(List<?> list) {
    HashSet<T> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      Object coerced = Polyglot_Utils.convertPolyglotValue(o);
      if (storedType.isInstance(coerced)) {
        set.add(storedType.cast(coerced));
      }
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
