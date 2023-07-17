package org.enso.exploratory_benchmark_helpers;

import java.time.LocalDate;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;

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

  public static String longestText(StringStorage storage) {
    long longest = -1;
    String longestText = null;
    int n = storage.size();
    for (int i = 0; i < n; i++) {
      if (!storage.isNa(i)) {
        String text = storage.getItem(i);
        long length = Text_Utils.grapheme_length(text);
        if (length > longest) {
          longest = length;
          longestText = text;
        }
      }
    }
    return longestText;
  }
}
