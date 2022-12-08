package org.enso.table.data.table.join.scan;

import org.enso.table.data.table.problems.AggregatedProblems;

public interface Matcher {
  boolean matches(int left, int right);

  default AggregatedProblems getProblems() {
    return AggregatedProblems.of();
  }
}
