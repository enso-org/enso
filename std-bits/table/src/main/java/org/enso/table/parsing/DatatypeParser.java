package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.read.WithProblems;

/**
 * A base type for a datatype parsing strategy.
 *
 * <p>It specifies the strategy for parsing text cells into some target type, reporting issues and
 * building the resulting table column.
 *
 * @param <PA> the specific problem aggregator type; the type is refined to be able to handle
 *     various strategies for aggregating problems, depending on the particular datatype
 */
public abstract class DatatypeParser<PA extends ProblemAggregator> {

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
  public abstract Object parseSingleValue(String text, PA problemAggregator);

  /**
   * Creates a new column builder expecting the specific datatype, with a specified capacity.
   *
   * <p>The {@code parseColumn} method will use {@code appendNoGrow} function, so the initial
   * capacity should be set properly so that the builder can hold all expected elements.
   *
   * <p>The type returned from {@code parseSingleValue} should be consistent with the types that the
   * builder returned here expects - it should never return a value that cannot be accepted by the
   * builder.
   */
  public abstract Builder makeBuilderWithCapacity(long capacity);

  /** Creates a new instance of the specific problem aggregator type. */
  public abstract PA makeProblemAggregator();

  /**
   * Parses a column of texts (represented as a {@code StringStorage}) and returns a new storage,
   * containing the parsed elements.
   */
  public WithProblems<Storage> parseColumn(StringStorage sourceStorage) {
    Builder builder = makeBuilderWithCapacity(sourceStorage.size());
    PA aggregator = makeProblemAggregator();

    for (int i = 0; i < sourceStorage.size(); ++i) {
      String cell = sourceStorage.getItem(i);
      if (cell != null) {
        Object parsed = parseSingleValue(cell, aggregator);
        builder.appendNoGrow(parsed);
      } else {
        builder.appendNoGrow(null);
      }
    }

    return new WithProblems<>(builder.seal(), aggregator.getAggregatedProblems());
  }
}
