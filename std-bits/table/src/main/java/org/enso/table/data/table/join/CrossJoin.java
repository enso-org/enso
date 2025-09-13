package org.enso.table.data.table.join;

import org.enso.base.ProgressReporter;
import org.enso.table.data.column.operation.StorageIterators;

public class CrossJoin {
  public static JoinResult perform(long leftRowCount, long rightRowCount) {
    long steps = leftRowCount * rightRowCount;
    var resultBuilder = new JoinResult.Builder(steps);
    try (var progressReporter =
        ProgressReporter.createWithStep("CrossJoin", steps, StorageIterators.PROGRESS_STEP)) {
      for (long l = 0; l < leftRowCount; ++l) {
        for (long r = 0; r < rightRowCount; ++r) {
          resultBuilder.addMatchedRowsPair(l, r);
          progressReporter.advance();
        }
      }
    }
    return resultBuilder.buildAndInvalidate();
  }
}
