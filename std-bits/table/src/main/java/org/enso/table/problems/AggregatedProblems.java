package org.enso.table.problems;

import org.enso.table.data.table.problems.ColumnAggregatedProblems;

import java.util.ArrayList;
import java.util.List;

public class AggregatedProblems {
  private final List<Problem> problems;
  private final int maxSize;
  private int count;

  private AggregatedProblems(List<Problem> problems, int count) {
    this.problems = problems;
    this.maxSize = problems.size();
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
}
