package org.enso.table.problems;

/**  A value annotated with aggregated problems that occurred when it was being computed. */
public record WithAggregatedProblems<T>(T value, AggregatedProblems problems) {
}
