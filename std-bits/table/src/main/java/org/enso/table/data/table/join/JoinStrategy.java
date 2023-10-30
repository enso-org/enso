package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;

/** A strategy used for performing a join of two tables. */
public interface JoinStrategy {
  JoinResult join(
      Table left, Table right, List<JoinCondition> conditions, ProblemAggregator problemAggregator);

  static JoinStrategy createStrategy(List<JoinCondition> conditions) {
    assert !conditions.isEmpty();
    boolean hasEquals = conditions.stream().anyMatch(c -> c instanceof Equals || c instanceof EqualsIgnoreCase);
    boolean hasBetween = conditions.stream().anyMatch(c -> c instanceof Between);

    assert hasEquals || hasBetween;
    if (hasEquals && hasBetween) {
      // TODO combined Index + Sort
      return new IndexJoin();
    } else if (hasEquals) {
      return new IndexJoin();
    } else {
      return new SortJoin();
    }
  }
}
