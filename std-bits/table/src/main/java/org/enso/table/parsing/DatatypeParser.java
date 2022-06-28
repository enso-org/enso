package org.enso.table.parsing;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.parsing.problems.ProblemAggregatorImpl;
import org.enso.table.problems.WithProblems;

/** A base type for a parser capable of parsing a column of text values into some other type. */
public abstract class DatatypeParser {
  /**
   * Parses a single cell.
   *
   * @param text the text contents to parse, it will never be null in the default implementation -
   *     null values are just passed as-is without any parsing attempts by default
   * @param problemAggregator an instance of the problem aggregator, used for reporting parsing
   *     problems
   * @return the parsed value or null if the value could not be parsed or could be parsed but should
   *     be treated as missing value
   */
  protected abstract Object parseSingleValue(String text, ProblemAggregator problemAggregator);

  /**
   * Parses a single value not contained in a column.
   *
   * <p>Any reported problems will contain {@code null} as the related column reference.
   */
  public WithProblems<Object> parseIndependentValue(String text) {
    ProblemAggregator problemAggregator = new ProblemAggregatorImpl(null);
    Object result = parseSingleValue(text, problemAggregator);
    return new WithProblems<>(result, problemAggregator.getAggregatedProblems());
  }

  /**
   * Parses a column of texts (represented as a {@code StringStorage}) and returns a new storage,
   * containing the parsed elements.
   */
  public abstract WithProblems<Storage> parseColumn(String columnName, StringStorage sourceStorage);
}
