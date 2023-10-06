package org.enso.table.problems;

import java.util.ArrayList;
import java.util.List;
import org.enso.table.data.table.problems.ColumnAggregatedProblem;

@Deprecated
public class AggregatedProblems {
  private final List<Problem> problems;
  private final int maxSize;
  private int count;

  private AggregatedProblems(List<Problem> problems, int count) {
    this.problems = problems;
    this.maxSize = problems.size() + 10;
    this.count = count;
  }

  public AggregatedProblems() {
    this(10);
  }

  public AggregatedProblems(int maxSize) {
    this.problems = new ArrayList<>();
    this.maxSize = maxSize;
    this.count = 0;
  }

  public Problem[] getProblems() {
    if (count == 0) {
      return new Problem[0];
    }

    return problems.toArray(Problem[]::new);
  }

  public List<Problem> asList() {
    return problems;
  }

  public int getCount() {
    return count;
  }

  public void add(Problem problem) {
    if (problem instanceof ColumnAggregatedProblem) {
      for (Problem p : problems) {
        if (p instanceof ColumnAggregatedProblem
            && ((ColumnAggregatedProblem) p).merge((ColumnAggregatedProblem) problem)) {
          return;
        }
      }
    }

    if (problems.size() < maxSize) {
      problems.add(problem);
    }
    count++;
  }

  public void addAll(List<Problem> problems) {
    for (Problem p : problems) {
      add(p);
    }
  }

  public static AggregatedProblems merge(AggregatedProblems... problems) {
    List<Problem> merged = new ArrayList<>();
    int count = 0;

    for (AggregatedProblems p : problems) {
      if (p != null && p.count > 0) {
        merged.addAll(p.problems);
        count += p.getCount();
      }
    }

    return new AggregatedProblems(merged, count);
  }

  public static AggregatedProblems of(Problem... problems) {
    AggregatedProblems result = new AggregatedProblems();
    for (Problem p : problems) {
      result.add(p);
    }
    return result;
  }

  public static AggregatedProblems of(List<Problem> problems) {
    AggregatedProblems result = new AggregatedProblems();
    for (Problem p : problems) {
      result.add(p);
    }
    return result;
  }

  /**
   * A helper method for migration from {@link AggregatedProblems} to {@link ProblemAggregator} in
   * an easy way.
   *
   * @deprecated It will be removed together with {@link AggregatedProblems}, but can be used to
   *     perform migration in parts.
   */
  @Deprecated
  public static void addToAggregator(AggregatedProblems problems, ProblemAggregator aggregator) {
    if (problems == null) {
      return;
    }

    // Merely creating this class registers it to the parent and will ensure the problems will be
    // added when summarizing.
    new AggregatedProblemsProxyAggregator(aggregator, problems);
  }

  private static class AggregatedProblemsProxyAggregator extends ProblemAggregator {

    private final AggregatedProblems problemsToAdd;

    protected AggregatedProblemsProxyAggregator(
        ProblemAggregator parent, AggregatedProblems problemsToAdd) {
      super(parent);
      this.problemsToAdd = problemsToAdd;
    }

    @Override
    public ProblemSummary summarize() {
      var summary = super.summarize();
      summary.problems.addAll(problemsToAdd.problems);
      summary.allProblemsCount += problemsToAdd.count;
      return summary;
    }
  }
}
