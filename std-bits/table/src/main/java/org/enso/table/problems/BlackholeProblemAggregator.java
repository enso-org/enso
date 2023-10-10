package org.enso.table.problems;

public class BlackholeProblemAggregator extends ProblemAggregator {

  /**
   * An instance of ProblemAggregator that discards any problems.
   *
   * <p>It should be used rather rarely to avoid losing problems. But it may be useful when we are
   * doing some internal checks whose problems are never supposed to surface.
   */
  public static final BlackholeProblemAggregator INSTANCE = new BlackholeProblemAggregator();

  private BlackholeProblemAggregator() {
    super(new PrivateConstructorToken());
  }

  protected static class PrivateConstructorToken {
    private PrivateConstructorToken() {}
  }

  @Override
  public void report(Problem problem) {}

  @Override
  protected void registerChild(ProblemAggregator child) {}

  @Override
  public ProblemSummary summarize() {
    throw new IllegalStateException(
        "Summarize should never be called on BlackholeProblemAggregator.");
  }
}
