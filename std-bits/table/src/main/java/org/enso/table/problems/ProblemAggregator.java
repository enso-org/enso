package org.enso.table.problems;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.graalvm.polyglot.Context;

/**
 * The ProblemAggregator is the main way for reporting warnings from helper Java code to Enso.
 *
 * <p>The Enso user should always use the `Java_Problems.with_problem_aggregator` helper to get an
 * instance of ProblemAggregator and pass it to any Java code that requires it.
 *
 * <p>Child instances of the aggregator can be constructed via various means. They can be used to
 * detach various warnings in case a branch of the computation fails, or to add specialized logic
 * for aggregating various kinds of problems, or to provide additional context information to be
 * attached to the aggregated problems (e.g. {@link ColumnAggregatedProblemAggregator}).
 *
 * <p>We ensure that no problems are 'dropped' by requiring every method reporting problems to take
 * an instance of the ProblemAggregator. The ProblemAggregators create a hierarchy of child
 * aggregators that ensures all problems are passed upwards, up to the top-level aggregator created
 * in `with_problem_aggregator`. Thus, no problems are discarded.
 *
 * <p>The only thing the user has to be careful about is the lifetime of the aggregators. Once we
 * exit the `with_problem_aggregator` section, the aggregator is summarized and any reported
 * problems are reported in Enso. After that, no new problems can be reported, because they would be
 * lost. This is verified by the `checkNotFinished` method - if the user ever uses an aggregator
 * after summarizing, such code will throw an exception. In general, the `with_problem_aggregator`
 * section should encompass the whole chunk of operation that is being performed, to ensure that all
 * processing that may report problems to the aggregator is finished before we exit the section.
 */
public class ProblemAggregator {
  protected List<Problem> directlyReportedProblems = new ArrayList<>();
  protected List<ProblemAggregator> children = new ArrayList<>();
  protected final ProblemAggregator parent;
  protected boolean isFinished = false;

  protected void checkNotFinished() {
    if (isFinished) {
      throw new IllegalStateException(
          "This ProblemAggregator instance has already been summarized. Using "
              + "ProblemAggregator after it has been summarized is a bug, because the problems will be lost.");
    }
  }

  /* Called by any processing code to report a simple problem. Specialized implementations may be available too. */
  public void report(Problem problem) {
    checkNotFinished();
    directlyReportedProblems.add(problem);
  }

  /**
   * A summary that includes gathered problems and a count.
   *
   * <p>The count may be larger than the list size, meaning that some problems were dropped due to
   * count limits - it can be used to add an `Additional_Warnings` problem.
   */
  public static class ProblemSummary {
    public final List<Problem> problems;
    public long allProblemsCount;

    public ProblemSummary(List<Problem> problems, long allProblemsCount) {
      this.problems = problems;
      this.allProblemsCount = allProblemsCount;
    }

    public ProblemSummary add(Problem problem) {
      problems.add(problem);
      allProblemsCount += 1;
      return this;
    }
  }

  /* Called by the top-level user after all processing is completed, to summarize problems that happened. */
  public ProblemSummary summarize() {
    isFinished = true;
    List<Problem> problems = new ArrayList<>(directlyReportedProblems);
    long count = directlyReportedProblems.size();
    Context context = Context.getCurrent();
    for (ProblemAggregator child : children) {
      ProblemSummary childSummary = child.summarize();
      problems.addAll(childSummary.problems);
      count += childSummary.allProblemsCount;
      context.safepoint();
    }

    return new ProblemSummary(problems, count);
  }

  protected void registerChild(ProblemAggregator child) {
    checkNotFinished();
    children.add(child);
  }

  /* The simple constructor is private, so children need to use one that specifies the parent, thus guaranteeing that
  a parent exists. */
  private ProblemAggregator() {
    parent = null;
  }

  /* A special constructor that can be used by BlackholeProblemAggregator to create its own instance without a
  parent, only the Blackhole can create this token so no other implementation is allowed to use it - thus
  guaranteeing that all other implementations are forced to specify a parent. */
  protected ProblemAggregator(BlackholeProblemAggregator.PrivateConstructorToken token) {
    Objects.requireNonNull(token);
    parent = null;
  }

  /* The constructor to use for inheritors, that guarantees that it is attached to a parent. */
  protected ProblemAggregator(ProblemAggregator parent) {
    Objects.requireNonNull(parent);
    this.parent = parent;
    parent.registerChild(this);
  }

  /**
   * This should only be used by top-level code, and any call to this method should be paired up
   * with a summarize call that translates problems from Java to Enso.
   *
   * <p>It should only be called by `with_problem_aggregator`. It should never be called directly
   * from Java.
   *
   * @deprecated This method is actually not deprecated, it is just marked as such to avoid any
   *     usages from the Java code (they will generate warnings, whereas the only allowed Enso usage
   *     will not).
   */
  @Deprecated(forRemoval = false)
  public static ProblemAggregator makeTopLevelAggregator() {
    return new ProblemAggregator();
  }

  /**
   * This method may be called to avoid passing problems from this aggregator to its parent, when
   * summarize is called.
   *
   * <p>All aggregators pass their problems upstream by default, but we can decide to opt-out of
   * this, for example when performing 'backtracking' and rolling back a failed branch.
   */
  public void detachFromParent() {
    if (parent == null) {
      throw new NullPointerException(
          "Cannot detach the top-level aggregator, because it has no parents.");
    }

    parent.children.remove(this);
  }

  /**
   * Creates a child aggregator that will forward all of its problems to the parent, unless it is
   * later detached.
   */
  public ProblemAggregator createSimpleChild() {
    return new ProblemAggregator(this);
  }
}
