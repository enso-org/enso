package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.read.WithProblems;

/**
 * The type inferring parser tries to parse the given column using a set of provided parsers. It
 * returns the result of the first parser that succeeds without reporting any problems.
 *
 * <p>At least one parser must always be provided. The last parser on the list is used as a fallback
 * - its result is returned regardless of whether it contained problems or not.
 */
public class TypeInferringParser implements DatatypeParser {

  private final IncrementalDatatypeParser[] baseParsers;

  public TypeInferringParser(IncrementalDatatypeParser[] baseParsers) {
    if (baseParsers.length == 0) {
      throw new IllegalArgumentException("At least one parser must be provided.");
    }
    this.baseParsers = baseParsers;
  }

  @Override
  public WithProblems<Storage> parseColumn(StringStorage sourceStorage) {
    parsers: for (int i = 0; i < baseParsers.length; ++i) {
      boolean isLast = i == baseParsers.length - 1;

      IncrementalDatatypeParser parser = baseParsers[i];
      Builder builder = parser.makeBuilderWithCapacity(sourceStorage.size());
      var aggregator = new ProblemAggregator();

      for (int j = 0; j < sourceStorage.size(); ++j) {
        String cell = sourceStorage.getItem(j);
        if (cell != null) {
          Object parsed = parser.parseSingleValue(cell, aggregator);
          if (aggregator.hasProblems() && !isLast) {
            continue parsers;
          }
          builder.appendNoGrow(parsed);
        } else {
          builder.appendNoGrow(null);
        }
      }

      return new WithProblems<>(builder.seal(), aggregator.getAggregatedProblems());
    }

    throw new IllegalStateException("`baseParsers` should not be empty.");
  }
}
