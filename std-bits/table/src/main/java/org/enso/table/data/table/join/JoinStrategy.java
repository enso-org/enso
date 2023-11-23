package org.enso.table.data.table.join;

import org.enso.table.data.table.join.between.SortJoin;
import org.enso.table.data.table.join.conditions.Between;
import org.enso.table.data.table.join.conditions.Equals;
import org.enso.table.data.table.join.conditions.EqualsIgnoreCase;
import org.enso.table.data.table.join.conditions.HashableCondition;
import org.enso.table.data.table.join.conditions.JoinCondition;
import org.enso.table.data.table.join.hashing.HashJoin;
import org.enso.table.problems.ProblemAggregator;

import java.util.List;

/**
 * A strategy used for performing a join of two tables.
 */
public interface JoinStrategy {
  JoinResult join(ProblemAggregator problemAggregator);

  static JoinStrategy createStrategy(List<JoinCondition> conditions) {
    if (conditions.isEmpty()) {
      throw new IllegalArgumentException("At least one join condition must be provided.");
    }

    List<HashableCondition> hashableConditions = conditions.stream()
        .filter(c -> c instanceof HashableCondition)
        .map(c -> (HashableCondition) c)
        .toList();
    List<Between> betweenConditions = conditions.stream()
        .filter(c -> c instanceof Between)
        .map(c -> (Between) c)
        .toList();

    if (hashableConditions.size() + betweenConditions.size() != conditions.size()) {
      throw new IllegalArgumentException("Unsupported join condition.");
    }

    if (hashableConditions.isEmpty()) {
      assert !betweenConditions.isEmpty();
      return new SortJoin(betweenConditions);
    } else if (betweenConditions.isEmpty()) {
      return new HashJoin(hashableConditions, new MatchAllStrategy());
    } else {
      return new HashJoin(hashableConditions, new SortJoin(betweenConditions));
    }
  }

  class ConditionsHelper {
    private final List<? extends JoinCondition> conditions;

    public ConditionsHelper(List<? extends JoinCondition> conditions) {
      if (conditions.isEmpty()) {
        throw new IllegalArgumentException("At least one join condition must be provided.");
      }

      this.conditions = conditions;
    }

    public int getLeftTableRowCount() {
      return switch (conditions.get(0)) {
        case Equals equals -> equals.left().getStorage().size();
        case EqualsIgnoreCase equalsIgnoreCase -> equalsIgnoreCase.left().getStorage().size();
        case Between between -> between.left().getStorage().size();
      };
    }

    public int getRightTableRowCount() {
      return switch (conditions.get(0)) {
        case Equals equals -> equals.right().getStorage().size();
        case EqualsIgnoreCase equalsIgnoreCase -> equalsIgnoreCase.right().getStorage().size();
        case Between between -> between.rightLower().getStorage().size();
      };
    }
  }
}
