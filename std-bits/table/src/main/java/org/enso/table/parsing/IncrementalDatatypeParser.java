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
   * <p>The {@code parseColumn} knows the expected size, so the initial capacity should be set
   * properly to avoid reallocations.
   *
   * <p>The type returned from {@code parseSingleValue} should be consistent with the types that the
   * builder returned here expects - it should never return a value that cannot be accepted by the
   * builder.
   */
  protected abstract Builder makeBuilderWithCapacity(
      long capacity, ProblemAggregator problemAggregator);

  /**
   * Parses a column of texts (represented as a {@code Storage<String>}) and returns a new storage,
   * containing the parsed elements.
   */
  @Override
  public Storage<?> parseColumn(
      Storage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
    long size = sourceStorage.getSize();
    Builder builder = makeBuilderWithCapacity(size, problemAggregator);

    Context context = Context.getCurrent();
    for (long i = 0; i < size; ++i) {
      String cell = sourceStorage.getItemBoxed(i);
      if (cell != null) {
        Object parsed = parseSingleValue(cell, problemAggregator);
        builder.append(parsed);
      } else {
        builder.appendNulls(1);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
