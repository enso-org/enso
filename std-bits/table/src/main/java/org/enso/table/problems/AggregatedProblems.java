package org.enso.table.problems;

import org.enso.table.data.table.problems.ColumnAggregatedProblems;

import java.util.ArrayList;
import java.util.List;

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
    if (problem instanceof ColumnAggregatedProblems) {
      for (Problem p : problems) {
        if (p instanceof ColumnAggregatedProblems
            && ((ColumnAggregatedProblems) p).merge((ColumnAggregatedProblems) problem)) {
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
   * A helper method for migration from {@link AggregatedProblems} to {@link ProblemAggregator} in an easy way.
   *
   * @deprecated It will be removed together with {@link AggregatedProblems}, but can be used to perform migration in
   *     parts.
   */
  @Deprecated
  public void addToAggregator(ProblemAggregator aggregator) {
    // Merely creating this class registers it to the parent and will ensure the problems will be added when summarizing.
    new AggregatedProblemsProxyAggregator(aggregator, this);
  }

  private static class AggregatedProblemsProxyAggregator extends ProblemAggregator {

    private final AggregatedProblems problemsToAdd;

    protected AggregatedProblemsProxyAggregator(ProblemAggregator parent, AggregatedProblems problemsToAdd) {
      super(parent);
      this.problemsToAdd = problemsToAdd;
    }

    @Override
    public ProblemSummary summarize() {
      var baseSummary = super.summarize();
      List<Problem> problems = new ArrayList<>(baseSummary.problems());
      problems.addAll(problemsToAdd.problems);
      long count = baseSummary.allProblemsCount() + problemsToAdd.count;
      return new ProblemSummary(problems, count);
    }
  }
}
