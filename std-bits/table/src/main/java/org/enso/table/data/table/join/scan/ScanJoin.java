package org.enso.table.data.table.join.scan;

import org.enso.table.data.table.Table;
import org.enso.table.data.table.join.*;
import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ScanJoin implements JoinStrategy {
  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions) {
    List<Pair<Integer, Integer>> matches = new ArrayList<>();
    int ls = left.rowCount();
    int rs = right.rowCount();

    List<Matcher> matchers = conditions.stream().map(Matcher::create).collect(Collectors.toList());

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

    return new JoinResult(matches);
  }
}
