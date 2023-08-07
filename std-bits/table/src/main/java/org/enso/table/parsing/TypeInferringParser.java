package org.enso.table.parsing;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.parsing.problems.ProblemAggregatorImpl;
import org.enso.table.parsing.problems.SimplifiedProblemAggregator;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.problems.WithAggregatedProblems;
import org.graalvm.polyglot.Context;

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
  public WithAggregatedProblems<Storage<?>> parseColumn(
      String columnName, Storage<String> sourceStorage) {
    // If there are no values, the Auto parser would guess some random type (the first one that is
    // checked). Instead, we just return the empty column unchanged.
    boolean hasNoValues =
        (sourceStorage.size() == 0) || (sourceStorage.countMissing() == sourceStorage.size());
    if (hasNoValues) {
      return fallbackParser.parseColumn(columnName, sourceStorage);
    }

    Context context = Context.getCurrent();
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

        context.safepoint();
      }

      return new WithAggregatedProblems<>(
          builder.seal(),
          AggregatedProblems.merge(aggregator.getAggregatedProblems(), builder.getProblems()));
    }

    return fallbackParser.parseColumn(columnName, sourceStorage);
  }
}
