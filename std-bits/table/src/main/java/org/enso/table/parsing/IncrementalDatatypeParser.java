package org.enso.table.parsing;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A base type for a datatype parsing strategy which relies on a method parsing a single value.
 *
 * <p>It specifies the strategy for parsing text cells into some target type, reporting issues and
 * building the resulting table column.
 */
public abstract class IncrementalDatatypeParser extends DatatypeParser {
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
  protected abstract Builder makeBuilderWithCapacity(
      int capacity, ProblemAggregator problemAggregator);

  /**
   * Parses a column of texts (represented as a {@code StringStorage}) and returns a new storage,
   * containing the parsed elements.
   */
  @Override
  public Storage<?> parseColumn(
      Storage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
    Builder builder = makeBuilderWithCapacity(sourceStorage.size(), problemAggregator);

    Context context = Context.getCurrent();
    for (int i = 0; i < sourceStorage.size(); ++i) {
      String cell = sourceStorage.getItemBoxed(i);
      if (cell != null) {
        Object parsed = parseSingleValue(cell, problemAggregator);
        builder.appendNoGrow(parsed);
      } else {
        builder.appendNoGrow(null);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
