package org.enso.table.data.table.join.scan;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.join.*;
import org.graalvm.collections.Pair;

public class ScanJoin implements JoinStrategy {

  private final Comparator<Object> objectComparator;
  private final BiFunction<Object, Object, Boolean> equalityFallback;

  public ScanJoin(
      Comparator<Object> objectComparator, BiFunction<Object, Object, Boolean> equalityFallback) {
    this.objectComparator = objectComparator;
    this.equalityFallback = equalityFallback;
  }

  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions) {
    int ls = left.rowCount();
    int rs = right.rowCount();

    MatcherFactory factory = new MatcherFactory(objectComparator, equalityFallback);
    Matcher compoundMatcher = factory.create(conditions);

    JoinResult.Builder resultBuilder = new JoinResult.Builder();

    for (int l = 0; l < ls; ++l) {
      for (int r = 0; r < rs; ++r) {
        if (compoundMatcher.matches(l, r)) {
          resultBuilder.addRow(l, r);
        }
      }
    }

    return resultBuilder.build(compoundMatcher.getProblems());
  }
}
