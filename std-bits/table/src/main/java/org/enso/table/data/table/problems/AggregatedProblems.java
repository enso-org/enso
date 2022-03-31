package org.enso.table.data.table.problems;

import java.util.Arrays;
import java.util.stream.Stream;

public class AggregatedProblems {
  private final Problem[] problems;
  private int count = 0;

  private AggregatedProblems(Problem[] problems, int count) {
    this.problems = problems;
    this.count = count;
  }

  public AggregatedProblems(int size) {
    this.problems = new Problem[size];
  }

  public AggregatedProblems() {
    this(10);
  }

  public Problem[] getProblems() {
    if (count == 0) {
      return new Problem[0];
    } else if (count == problems.length) {
      return problems;
    }

    return Arrays.copyOfRange(problems, 0, count);
  }

  public int getCount() {
    return count;
  }

  public void add(Problem problem) {
    if (count < problems.length) {
      problems[count] = problem;
    }
    count++;
  }

  public static AggregatedProblems merge(AggregatedProblems[] problems) {
    int size = Arrays.stream(problems).mapToInt(p->p == null ? 0 : p.problems.length).sum();
    Problem[] merged = new Problem[size];

    int count = 0;
    int position = 0;
    for (AggregatedProblems p : problems) {
      if (p != null) {
        int toCopy = Math.min(p.problems.length, p.getCount());
        System.arraycopy(p.problems, 0, merged, position, toCopy);
        position += toCopy;

        count += p.getCount();
      }
    }

    return new AggregatedProblems(merged, count);
  }
}
