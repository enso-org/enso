package org.enso.table.parsing.problems;

import java.util.HashSet;
import java.util.Set;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

/**
 * A {@link org.enso.table.parsing.problems.ParseProblemAggregator} implementation that also
 * forwards problems to the parent {@link org.enso.table.problems.ProblemAggregator}.
 */
public final class CommonParseProblemAggregator extends ProblemAggregator
    implements ParseProblemAggregator {
  public final String relatedColumnName;

  // Used for the InvalidFormat error
  public final Value expectedEnsoValueType;
  private int invalidFormatCount = 0;
  private final Set<String> invalidFormatCells = new HashSet<>();

  public CommonParseProblemAggregator(
      ProblemAggregator parent, String relatedColumnName, Value expectedEnsoValueType) {
    super(parent);
    this.relatedColumnName = relatedColumnName;
    this.expectedEnsoValueType = expectedEnsoValueType;
  }

  @Override
  public void reportInvalidFormat(String cell) {
    invalidFormatCount++;
    invalidFormatCells.add(cell);
  }

  @Override
  public void reportMismatchedQuote(String cellText) {
    throw new MismatchedQuote(cellText);
  }

  @Override
  public boolean hasProblems() {
    return !invalidFormatCells.isEmpty();
  }

  @Override
  public ProblemSummary summarize() {
    ProblemSummary baseSummary = super.summarize();

    if (!invalidFormatCells.isEmpty()) {
      baseSummary.add(
          new InvalidFormat(
              relatedColumnName,
              expectedEnsoValueType,
              invalidFormatCount,
              invalidFormatCells.stream().toList()));
    }

    return baseSummary;
  }

  public CommonParseProblemAggregator createContextAwareChild() {
    return new CommonParseProblemAggregator(this, relatedColumnName, expectedEnsoValueType);
  }
}
