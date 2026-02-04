package org.enso.table.problems;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/**
 * The ProblemAggregator is the main way for reporting warnings from helper Java code to Enso.
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
 * <p>When using a ProblemAggregator from Enso, the user should use the `with_problem_aggregator`
 * construct, which creates a top-level ProblemAggregator instance, and then passes it to the
 * action. It will handle summarizing the problems and raising them or attaching them to values as
 * needed.
 *
 * <p>Once the aggregator has been summarized, it cannot be used anymore - any attempt to report
 * problems to it will result in an exception. This is to avoid losing problems by accident.
 */
public class ProblemAggregator {
  protected List<Problem> directlyReportedProblems = new ArrayList<>();
  protected List<ProblemAggregator> children = new ArrayList<>();
  protected final ProblemAggregator parent;
  protected boolean isFinished = false;

  protected void checkNotFinished() {
    if (isFinished) {
      throw new IllegalStateException(
          "This ProblemAggregator instance has already been summarized. Using ProblemAggregator"
              + " after it has been summarized is a bug, because the problems will be lost.");
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
  ProblemAggregator(BlackholeProblemAggregator.PrivateConstructorToken token) {
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
   */
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

  /**
   * Summarizes the problems and, if there are any errors, returns the first one as a dataflow
   * error. Otherwise, the original value is returned.
   *
   * @param value the value to attach problems to
   * @return the value with attached problems, or the original value if there are no problems
   */
  public final Value throwIfAnyErrors(Value value) {
    ProblemSummary summary = summarize();
    var error = summary.problems.stream().filter(Problem::isError).findFirst();
    return error.map(problem -> EnsoMeta.asDataflowError(problem.asEnsoValue())).orElse(value);
  }

  /**
   * Attaches the problems summarized by this aggregator to the given value.
   *
   * <p>If there are no problems, the original value is returned. Otherwise, a new value with
   * attached problems is returned.
   *
   * @param value the value to attach problems to
   * @param asErrors whether to attach problems as errors (if true) or warnings (if false)
   * @return the value with attached problems, or the original value if there are no problems
   */
  public final Value attachProblemsToValue(Value value, boolean asErrors) {
    ProblemSummary summary = summarize();
    if (summary.allProblemsCount == 0) {
      return value;
    }

    // Check for any errors or if asErrors raise the first problem.
    var firstError = summary.problems.stream().filter(p -> asErrors || p.isError()).findFirst();
    if (firstError.isPresent()) {
      return EnsoMeta.asDataflowError(firstError.get().asEnsoValue());
    }

    // Create a list of problems to attach.
    var ensoProblems = summary.problems.stream().map(Problem::asEnsoValue).toList();
    if (ensoProblems.size() != summary.allProblemsCount) {
      // Add an Additional_Warnings problem if some problems were dropped.
      var additionalWarnings =
          EnsoMeta.makeInstance(
              "Standard.Base.Errors.Common",
              "Additional_Warnings",
              "Error",
              summary.allProblemsCount - ensoProblems.size());
      ensoProblems = new ArrayList<>(ensoProblems);
      ensoProblems.add(additionalWarnings);
    }

    return EnsoMeta.getType("Standard.Base.Warning", "Warning")
        .invokeMember("attach_multiple", ensoProblems, value);
  }
}
