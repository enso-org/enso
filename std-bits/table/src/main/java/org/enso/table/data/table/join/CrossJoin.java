package org.enso.table.data.table.join;

import org.graalvm.polyglot.Context;

public class CrossJoin {
  public static JoinResult perform(int leftRowCount, int rightRowCount) {
    Context context = Context.getCurrent();
    JoinResult.Builder resultBuilder = new JoinResult.Builder(leftRowCount * rightRowCount);
    for (int l = 0; l < leftRowCount; ++l) {
      for (int r = 0; r < rightRowCount; ++r) {
        resultBuilder.addMatchedRowsPair(l, r);
        context.safepoint();
      }
    }
    return resultBuilder.buildAndInvalidate();
  }
}
