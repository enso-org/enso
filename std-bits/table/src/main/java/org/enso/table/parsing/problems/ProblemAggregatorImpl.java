package org.enso.table.parsing.problems;

import java.util.ArrayList;
import java.util.List;

public class ProblemAggregatorImpl implements ProblemAggregator {
  public final String relatedColumnName;
  private final List<String> invalidFormatCells = new ArrayList<>();
  private final List<String> leadingZerosCells = new ArrayList<>();
  private int mismatchedQuotes = 0;

  public ProblemAggregatorImpl(String relatedColumnName) {
    this.relatedColumnName = relatedColumnName;
  }

  @Override
  public void reportInvalidFormat(String cell) {
    invalidFormatCells.add(cell);
  }

  @Override
  public void reportLeadingZeroes(String cell) {
    leadingZerosCells.add(cell);
  }

  @Override
  public void reportMismatchedQuote() {
    mismatchedQuotes++;
  }

  @Override
  public boolean hasProblems() {
    return !invalidFormatCells.isEmpty() || !leadingZerosCells.isEmpty() || mismatchedQuotes > 0;
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    List<ParsingProblem> problems = new ArrayList<>();

    if (!invalidFormatCells.isEmpty()) {
      problems.add(new InvalidFormat(relatedColumnName, invalidFormatCells));
    }

    if (!leadingZerosCells.isEmpty()) {
      problems.add(new LeadingZeros(relatedColumnName, leadingZerosCells));
    }

    for (int i = 0; i < mismatchedQuotes; ++i) {
      problems.add(new MismatchedQuote());
    }

    assert problems.isEmpty() == !hasProblems();

    return problems;
  }
}
