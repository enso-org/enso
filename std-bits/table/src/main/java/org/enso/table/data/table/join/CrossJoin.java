package org.enso.table.data.table.join;

import org.enso.table.util.ProgressHandler;

public class CrossJoin {
  public static JoinResult perform(long leftRowCount, long rightRowCount) {
    long steps = leftRowCount * rightRowCount;
    var resultBuilder = new JoinResult.Builder(steps);
    try (var progressHandle = ProgressHandler.init("CrossJoin", steps)) {
      for (long l = 0; l < leftRowCount; ++l) {
        for (long r = 0; r < rightRowCount; ++r) {
          resultBuilder.addMatchedRowsPair(l, r);
          progressHandle.advance();
        }
      }
    }
    return resultBuilder.buildAndInvalidate();
  }
}
