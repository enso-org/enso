package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.data.table.join.between.SortJoin;
import org.enso.table.data.table.join.conditions.Between;
import org.enso.table.data.table.join.conditions.HashableCondition;
import org.enso.table.data.table.join.conditions.JoinCondition;
import org.enso.table.data.table.join.hashing.CompoundHashJoin;
import org.enso.table.data.table.join.hashing.SimpleHashJoin;
import org.enso.table.problems.ProblemAggregator;

/** A strategy used for performing a join of two tables. */
public interface JoinStrategy {
  JoinResult join(ProblemAggregator problemAggregator);

  static JoinStrategy createStrategy(List<JoinCondition> conditions, JoinKind joinKind) {
    ensureConditionsNotEmpty(conditions);

    List<HashableCondition> hashableConditions =
        conditions.stream()
            .filter(c -> c instanceof HashableCondition)
            .map(c -> (HashableCondition) c)
            .toList();
    List<Between> betweenConditions =
        conditions.stream().filter(c -> c instanceof Between).map(c -> (Between) c).toList();

    if (hashableConditions.size() + betweenConditions.size() != conditions.size()) {
      throw new IllegalArgumentException("Unsupported join condition.");
    }

    if (hashableConditions.isEmpty()) {
      assert !betweenConditions.isEmpty();
      return new SortJoin(betweenConditions, joinKind);
    } else if (betweenConditions.isEmpty()) {
      return new SimpleHashJoin(hashableConditions, joinKind);
    } else {
      return new CompoundHashJoin(hashableConditions, betweenConditions, joinKind);
    }
  }

  static void ensureConditionsNotEmpty(List<? extends JoinCondition> conditions) {
    if (conditions.isEmpty()) {
      throw new IllegalArgumentException("At least one join condition must be provided.");
    }
  }
}
