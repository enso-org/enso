package org.enso.table.data.column.operation;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.MixedStorage;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class RequiresNumberFormatting {
  /** Counts the number of cells in the columns with non trivial whitespace */
  public static boolean apply(Column column) throws InterruptedException {
    ColumnStorage storage = column.getStorage();
    return applyToStorage(storage);
  }

  /**
   * Counts the number of cells in the given storage with non trivial whitespace
   *
   * @return
   */
  public static boolean applyToStorage(ColumnStorage storage) throws InterruptedException {
    return (storage instanceof MixedStorage mixedStorage)
        ? mixedStorage.cachedNumericFormatCheck()
        : (boolean) compute(storage, Context.getCurrent());
  }

  /** Internal method performing the calculation on a storage. */
  public static boolean compute(ColumnStorage storage, Context context) {
    long size = storage.getSize();

    long count = 0;
    for (long i = 0; i < storage.getSize(); i++) {
      var val = storage.getItemBoxed(i);
      if (val instanceof Long n && (n > 999999 || n < -999999)) {
        return true;
      }

      if (context != null) {
        context.safepoint();
      }
    }

    return false;
  }
}
