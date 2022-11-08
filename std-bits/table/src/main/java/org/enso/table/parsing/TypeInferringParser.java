package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.parsing.problems.ProblemAggregatorImpl;
import org.enso.table.parsing.problems.SimplifiedProblemAggregator;
import org.enso.table.problems.WithProblems;

/**
 * The type inferring parser tries to parse the given column using a set of provided parsers. It
 * returns the result of the first parser that succeeds without reporting any problems.
 *
 * <p>If all parsers from the set reported problems, the fallback parser is used and its result is
 * returned regardless of any problems.
 */
public class TypeInferringParser extends DatatypeParser {

  private final IncrementalDatatypeParser[] baseParsers;
  private final DatatypeParser fallbackParser;

  public TypeInferringParser(
      IncrementalDatatypeParser[] baseParsers, DatatypeParser fallbackParser) {
    this.baseParsers = baseParsers;
    this.fallbackParser = fallbackParser;
  }

  @Override
  public Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    for (IncrementalDatatypeParser parser : baseParsers) {
      SimplifiedProblemAggregator internal = new SimplifiedProblemAggregator();
      Object result = parser.parseSingleValue(text, internal);
      if (!internal.hasProblems()) {
        return result;
      }
    }

    return fallbackParser.parseSingleValue(text, problemAggregator);
  }

  @Override
  public WithProblems<Storage<?>> parseColumn(String columnName, Storage<String> sourceStorage) {
    parsers:
    for (IncrementalDatatypeParser parser : baseParsers) {
      Builder builder = parser.makeBuilderWithCapacity(sourceStorage.size());
      var aggregator = new ProblemAggregatorImpl(columnName);

      for (int i = 0; i < sourceStorage.size(); ++i) {
        String cell = sourceStorage.getItemBoxed(i);
        if (cell != null) {
          Object parsed = parser.parseSingleValue(cell, aggregator);
          if (aggregator.hasProblems()) {
            continue parsers;
          }
          builder.appendNoGrow(parsed);
        } else {
          builder.appendNoGrow(null);
        }
      }

      return new WithProblems<>(builder.seal(), aggregator.getAggregatedProblems());
    }

    return fallbackParser.parseColumn(columnName, sourceStorage);
  }
}
