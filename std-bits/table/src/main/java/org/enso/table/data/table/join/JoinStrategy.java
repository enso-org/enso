package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;

public interface JoinStrategy {
  JoinResult join(
      Table left, Table right, List<JoinCondition> conditions, ProblemAggregator problemAggregator);
}
