package org.enso.table.parsing.problems;

import java.util.List;

/**
 * A base class for strategies for aggregating problems.
 *
 * <p>Each strategy exposes a method that returns a summary of the problems. The particular methods
 * for reporting each problem are defined in particular subclasses.
 */
public interface ProblemAggregator {
  /** Return an aggregated summary of problems that have been reported. */
  List<ParsingProblem> getAggregatedProblems();
}
