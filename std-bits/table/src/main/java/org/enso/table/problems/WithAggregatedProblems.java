package org.enso.table.problems;

/**  A value annotated with aggregated problems that occurred when it was being computed. */
@Deprecated
public record WithAggregatedProblems<T>(T value, AggregatedProblems problems) {
}
