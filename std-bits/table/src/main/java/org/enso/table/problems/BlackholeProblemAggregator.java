package org.enso.table.problems;

import java.util.List;

public class BlackholeProblemAggregator extends ProblemAggregator {

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
  public void reportAll(List<Problem> problems) {}

  @Override
  protected void registerChild(ProblemAggregator child) {}

  @Override
  public ProblemSummary summarize() {
    throw new IllegalStateException("Summarize should never be called on BlackholeProblemAggregator.");
  }
}
