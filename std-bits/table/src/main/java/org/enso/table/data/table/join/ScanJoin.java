package org.enso.table.data.table.join;

import org.enso.table.data.table.Table;
import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.List;

public class ScanJoin implements JoinStrategy {
  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions) {
    List<Pair<Integer, Integer>> matches = new ArrayList<>();
    int ls = left.rowCount();
    int rs = right.rowCount();
    for (int l = 0; l < ls; ++l) {
      for (int r = 0; r < rs; ++r) {
        boolean match = true;
        conditions: for (JoinCondition condition : conditions) {
          switch (condition) {
            case Equals eq -> {
              Object leftValue = eq.left().getStorage().getItemBoxed(l);
              Object rightValue = eq.right().getStorage().getItemBoxed(r);
              // TODO normalize equality of strings and decimals with ints
              if (!leftValue.equals(rightValue)) {
                match = false;
                break conditions;
              }
            }
            default -> throw new UnsupportedOperationException("Unsupported join condition: " + condition);
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
