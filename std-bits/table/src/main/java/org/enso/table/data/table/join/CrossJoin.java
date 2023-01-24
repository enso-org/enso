package org.enso.table.data.table.join;

import org.enso.table.problems.AggregatedProblems;

public class CrossJoin {
  public static JoinResult perform(int leftRowCount, int rightRowCount) {
    JoinResult.Builder resultBuilder = new JoinResult.Builder(leftRowCount * rightRowCount);
    for (int l = 0; l < leftRowCount; ++l) {
      for (int r = 0; r < rightRowCount; ++r) {
        resultBuilder.addRow(l, r);
      }
    }

    return resultBuilder.build(AggregatedProblems.of());
  }
}
