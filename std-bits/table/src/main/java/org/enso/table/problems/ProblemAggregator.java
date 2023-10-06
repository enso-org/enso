package org.enso.table.problems;

import java.util.ArrayList;
import java.util.List;

public class ProblemAggregator {
  protected List<Problem> directlyReportedProblems = new ArrayList<>();
  protected List<ProblemAggregator> children = new ArrayList<>();

  /* Called by any processing code to report a simple problem. Specialized implementations may be available too. */
  public void report(Problem problem) {
    directlyReportedProblems.add(problem);
  }

  /** A helper method, I'm not sure if we should have it, but it helps during migration. */
  @Deprecated
  public void reportAll(List<Problem> problems) {
    directlyReportedProblems.addAll(problems);
  }

  /**
   * A summary that includes gathered problems and a count.
   * <p>
   * The count may be larger than the list size, meaning that some problems were dropped due to count limits - it can be
   * used to add an `Additional_Warnings` problem.
   */
  public record ProblemSummary(List<Problem> problems, long allProblemsCount) {
  }

  /* Called by the top-level user after all processing is completed, to summarize problems that happened. */
  public ProblemSummary summarize() {
    List<Problem> problems = new ArrayList<>(directlyReportedProblems);
    long count = directlyReportedProblems.size();
    for (ProblemAggregator child : children) {
      ProblemSummary childSummary = child.summarize();
      problems.addAll(childSummary.problems);
      count += childSummary.allProblemsCount;
    }

    return new ProblemSummary(problems, count);
  }

  /* The simple constructor is private, so children need to use one that specifies the parent, thus guaranteeing that
   a parent exists. */
  private ProblemAggregator() {
  }

  /* The constructor to use for inheritors, that guarantees that it is attached to a parent. */
  protected ProblemAggregator(ProblemAggregator parent) {
    parent.children.add(this);
  }

  /* This should only be used by top-level code, and any call to this method should be paired up with a summarize
   * call that translates problems from Java to Enso.
   *
   * It should only be called by `with_problem_aggregator`.
   * It should never be called directly from Java.
   *
   * @deprecated This method is actually not deprecated, it is just marked as such to avoid any usages from the Java
   * code (they will generate warnings, whereas the only allowed Enso usage will not).
   */
  @Deprecated(forRemoval = false)
  public static ProblemAggregator makeTopLevelAggregator() {
    return new ProblemAggregator();
  }
}
