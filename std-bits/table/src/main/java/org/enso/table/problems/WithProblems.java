package org.enso.table.problems;

import java.util.List;

/**  A value annotated with problems that occurred when it was being computed. */
public record WithProblems<T>(T value, List<Problem> problems) {}
