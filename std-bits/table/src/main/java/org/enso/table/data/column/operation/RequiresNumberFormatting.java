package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.NumericFormattingStorage;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class RequiresNumberFormatting {
  /**
   * Indicates whether a column contains numbers greater than 1000000, and require formatting in viz
   */
  public static boolean apply(Column column) throws InterruptedException {
    ColumnStorage storage = column.getStorage();
    return applyToStorage(storage);
  }

  /**
   * Indicates whether a column contains numbers greater than 1000000
   *
   * @return true/false if the column contains large numbers
   */
  public static boolean applyToStorage(ColumnStorage storage) throws InterruptedException {
    if (storage instanceof NumericFormattingStorage numericStorage) {
      return numericStorage.cachedNumericFormatCheck();
    }
    return (boolean) compute(storage, Context.getCurrent());
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
      if (val instanceof Double n && (n > 999999 || n < -999999)) {
        return true;
      }
      if (val instanceof BigInteger n
          && (n.compareTo(BigInteger.valueOf(999999)) > 0
              || n.compareTo(BigInteger.valueOf(-999999)) < 0)) {
        return true;
      }
      if (val instanceof BigDecimal n
          && (n.compareTo(BigDecimal.valueOf(999999)) > 0
              || n.compareTo(BigDecimal.valueOf(-999999)) < 0)) {
        return true;
      }
      if (context != null) {
        context.safepoint();
      }
    }

    return false;
  }
}
