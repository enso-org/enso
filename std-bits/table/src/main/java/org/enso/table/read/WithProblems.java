package org.enso.table.read;

import java.util.List;
import org.enso.table.parsing.problems.ParsingProblem;

/**  A value annotated with problems that occurred when it was being computed. */
public record WithProblems<T>(T value, List<ParsingProblem> problems) {}
