package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.NumericFormattingStorage;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class RequiresNumberFormatting {

  private static final int FORMAT_NUMBER_LIMIT = 999999;

  /**
   * Indicates whether a column contains numbers greater than 1000000, and require formatting in viz
   */
  public static boolean apply(Column column) throws InterruptedException {
    return applyToStorage(column.getStorage());
  }

  /**
   * Indicates whether a column contains numbers greater than 1000000
   *
   * @return true/false if the column contains large numbers
   */
  public static boolean applyToStorage(ColumnStorage<?> storage) throws InterruptedException {
    if (storage instanceof NumericFormattingStorage numericStorage) {
      return numericStorage.cachedNumericFormatCheck();
    }
    return (boolean) compute(storage, Context.getCurrent());
  }

  /** Internal method performing the calculation on a storage. */
  public static boolean compute(ColumnStorage<?> storage, Context context) {
    for (long i = 0; i < storage.getSize(); i++) {
      var val = storage.getItemBoxed(i);
      if (val == null) {
        continue;
      }

      switch (val) {
        case Long n -> {
          return (n > FORMAT_NUMBER_LIMIT || n < -FORMAT_NUMBER_LIMIT);
        }
        case Double n -> {
          return (n > FORMAT_NUMBER_LIMIT || n < -FORMAT_NUMBER_LIMIT);
        }
        case BigInteger n -> {
          return (n.compareTo(BigInteger.valueOf(FORMAT_NUMBER_LIMIT)) > 0
              || n.compareTo(BigInteger.valueOf(-FORMAT_NUMBER_LIMIT)) < 0);
        }
        case BigDecimal n -> {
          return (n.compareTo(BigDecimal.valueOf(FORMAT_NUMBER_LIMIT)) > 0
              || n.compareTo(BigDecimal.valueOf(-FORMAT_NUMBER_LIMIT)) < 0);
        }
        default -> {
          return false;
        }
      }
    }

    return false;
  }
}
