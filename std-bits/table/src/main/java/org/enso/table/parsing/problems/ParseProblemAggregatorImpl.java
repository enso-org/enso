package org.enso.table.parsing.problems;

import java.util.ArrayList;
import java.util.List;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

public final class ParseProblemAggregatorImpl extends ProblemAggregator
    implements ParseProblemAggregator {
  public final String relatedColumnName;

  // Used for the InvalidFormat error
  public final Value expectedEnsoValueType;
  private final List<String> invalidFormatCells = new ArrayList<>();

  public ParseProblemAggregatorImpl(
      ProblemAggregator parent, String relatedColumnName, Value expectedEnsoValueType) {
    super(parent);
    this.relatedColumnName = relatedColumnName;
    this.expectedEnsoValueType = expectedEnsoValueType;
  }

  @Override
  public void reportInvalidFormat(String cell) {
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
          new InvalidFormat(relatedColumnName, expectedEnsoValueType, invalidFormatCells));
    }

    return baseSummary;
  }

  public ParseProblemAggregatorImpl createContextAwareChild() {
    return new ParseProblemAggregatorImpl(this, relatedColumnName, expectedEnsoValueType);
  }
}
