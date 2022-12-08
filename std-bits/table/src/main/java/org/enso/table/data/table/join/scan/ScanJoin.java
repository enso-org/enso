package org.enso.table.data.table.join.scan;

import org.enso.table.data.table.Table;
import org.enso.table.data.table.join.*;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

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
    List<Pair<Integer, Integer>> matches = new ArrayList<>();
    int ls = left.rowCount();
    int rs = right.rowCount();

    MatcherFactory factory = new MatcherFactory(objectComparator, equalityFallback);
    List<Matcher> matchers = conditions.stream().map(factory::create).collect(Collectors.toList());

    for (int l = 0; l < ls; ++l) {
      for (int r = 0; r < rs; ++r) {
        boolean match = true;
        for (Matcher matcher : matchers) {
          if (!matcher.matches(l, r)) {
            match = false;
            break;
          }
        }

        if (match) {
          matches.add(Pair.create(l, r));
        }
      }
    }

    AggregatedProblems problems =
        AggregatedProblems.merge(
            matchers.stream().map(Matcher::getProblems).toArray(AggregatedProblems[]::new));
    return new JoinResult(matches, problems);
  }
}
