package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;
import org.enso.table.problems.ProblemAggregator;

import java.util.ArrayList;
import java.util.List;

public final class ParseProblemAggregatorImpl extends ProblemAggregator implements ParseProblemAggregator {
  public final String relatedColumnName;
  private final List<String> invalidFormatCells = new ArrayList<>();

  public ParseProblemAggregatorImpl(ProblemAggregator parent, String relatedColumnName) {
    super(parent);
    this.relatedColumnName = relatedColumnName;
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
      baseSummary.add(new InvalidFormat(relatedColumnName, invalidFormatCells));
    }

    return baseSummary;
  }
}
