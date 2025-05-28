package org.enso.table.data.column.operation;

import java.util.HashSet;
import java.util.Set;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class DistinctValuesCheck {
  /** Indicates if there are less than 100 distinct cells in the column */
  public static Boolean apply(Column column) throws InterruptedException {
    ColumnStorage<?> storage = column.getStorage();
    return applyToStorage(storage);
  }

  /**
   * Indicates if there are less than 100 distinct cells in the given storage
   *
   * @return
   */
  public static Boolean applyToStorage(ColumnStorage<?> storage) throws InterruptedException {
    return (storage instanceof StringStorage stringStorage)
        ? stringStorage.cachedDistinctValueCheck()
        : (boolean) compute(storage, Context.getCurrent());
  }

  /** Internal method performing the calculation on a storage. */
  public static Boolean compute(ColumnStorage<?> storage, Context context) {
    long size = storage.getSize();

    Set<Object> uniqueValues = new HashSet<>();

    for (int i = 0; i < size; i++) {
      Object val = storage.getItemBoxed(i);

      uniqueValues.add(val);

      if (uniqueValues.size() > 100) {
        return false;
      }
    }

    return true;
  }
}
