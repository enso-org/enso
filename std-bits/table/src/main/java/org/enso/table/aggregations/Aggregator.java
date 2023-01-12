package org.enso.table.aggregations;

import org.enso.table.problems.AggregatedProblems;
import org.enso.table.problems.Problem;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/** Interface used to define aggregate columns. */
public abstract class Aggregator {
  private final String name;
  private final int type;
  private AggregatedProblems problems;

  protected Aggregator(String name, int type) {
    this.name = name;
    this.type = type;
    this.problems = null;
  }

  /**
   * Return name of the new column
   *
   * @return Name of the new column.
   */
  public final String getName() {
    return name;
  }

  /**
   * Return type of the column
   *
   * @return The type of the new column.
   */
  public int getType() {
    return type;
  }

  public AggregatedProblems getProblems() {
    return problems;
  }

  /**
   * Compute the value for a set of rows
   *
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public Object aggregate(int[] indexes) {
    return this.aggregate(Arrays.stream(indexes).boxed().collect(Collectors.toList()));
  }

  /**
   * Compute the value for a set of rows
   *
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public abstract Object aggregate(List<Integer> indexes);

  protected void addProblem(Problem problem) {
    if (problems == null) {
      problems = new AggregatedProblems();
    }
    problems.add(problem);
  }
}
