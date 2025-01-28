package org.enso.table.data.column.operation;

import java.util.Random;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class CountUntrimmed {
  // Default seed for random number generation (no specific reason for this value, just stability on
  // result).
  private static final long RANDOM_SEED = 677280131;

  // Default sample size for counting untrimmed cells.
  public static final long DEFAULT_SAMPLE_SIZE = 10000;

  /** Counts the number of cells in the columns with leading or trailing whitespace. */
  public static Long apply(Column column, long sampleSize) throws InterruptedException {
    var storage = column.getStorage();
    return applyToStorage(storage, sampleSize);
  }

  /** Counts the number of cells in the given storage with leading or trailing whitespace. */
  public static Long applyToStorage(ColumnStorage<?> storage, long sampleSize)
      throws InterruptedException {
    return (sampleSize == DEFAULT_SAMPLE_SIZE && storage instanceof StringStorage stringStorage)
        ? stringStorage.cachedUntrimmedCount()
        : (Long) compute(storage, sampleSize, Context.getCurrent());
  }

  /** Internal method performing the calculation on a storage. */
  public static long compute(ColumnStorage<?> storage, long sampleSize, Context context) {
    long size = storage.getSize();

    long count = 0;
    if (sampleSize < size) {
      var rng = new Random(RANDOM_SEED);
      for (int i = 0; i < sampleSize; i++) {
        long idx = rng.nextLong(size);
        var val = storage.getItemBoxed(idx);
        if (val instanceof String str && Text_Utils.has_leading_trailing_whitespace(str)) {
          count++;
        }

        if (context != null) {
          context.safepoint();
        }
      }
      count = Math.min(size, (long) Math.ceil((double) count / sampleSize * size));
    } else {
      for (long i = 0; i < storage.getSize(); i++) {
        var val = storage.getItemBoxed(i);
        if (val instanceof String str && Text_Utils.has_leading_trailing_whitespace(str)) {
          count++;
        }

        if (context != null) {
          context.safepoint();
        }
      }
    }

    return count;
  }
}
