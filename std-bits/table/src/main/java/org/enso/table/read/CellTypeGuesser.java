package org.enso.table.read;

import org.enso.table.parsing.IncrementalDatatypeParser;
import org.enso.table.parsing.problems.ProblemAggregator;

/**
 * A helper class that can be used to check if given values can be treated as some more specific
 * types or will be parsed as just string values.
 */
public class CellTypeGuesser {
  private final IncrementalDatatypeParser[] specificTypeParsers;

  public CellTypeGuesser(IncrementalDatatypeParser[] specificTypeParsers) {
    this.specificTypeParsers = specificTypeParsers;
  }

  /**
   * Checks if the given cell can be parsed as having a more specific type than just text.
   *
   * <p>Returns true if the given cell can be parsed as some specific type, according to one of the
   * configured parsers. This tool is used for analysing contents of rows to infer the headers, so
   * in this context the {@code null} value is also treated as a 'specific type'.
   */
  public boolean isOfSpecificType(String cell) {
    if (cell == null) return true;

    for (var parser : specificTypeParsers) {
      ProblemAggregator problemAggregator = new ProblemAggregator(null);
      parser.parseSingleValue(cell, problemAggregator);
      if (!problemAggregator.hasProblems()) return true;
    }

    return false;
  }

  /**
   * The inverse of {@code isOfSpecificType} - returns true if the value cannot be parsed as a more
   * specific type and will just stay as a non-null text, and false otherwise.
   */
  public boolean isPlainText(String cell) {
    return !isOfSpecificType(cell);
  }
}
