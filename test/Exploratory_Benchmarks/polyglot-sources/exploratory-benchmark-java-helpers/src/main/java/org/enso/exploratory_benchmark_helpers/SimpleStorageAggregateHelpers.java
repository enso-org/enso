package org.enso.exploratory_benchmark_helpers;

import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;

import java.time.LocalDate;

public class SimpleStorageAggregateHelpers {
  public static long sumLongStorage(LongStorage storage) {
    long sum = 0;
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        sum += storage.getItem(i);
      }
    }
    return sum;
  }

  public static long sumMonthsOfDateStorage(DateStorage storage) {
    long sum = 0;
    for (LocalDate date : storage.getData()) {
      if (date != null) {
        sum += date.getMonthValue();
      }
    }
    return sum;
  }
}
