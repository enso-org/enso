package org.enso.table.parsing;

import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.problems.WithAggregatedProblems;

/** A parser that just returns its input. Useful as a fallback. */
public class IdentityParser extends IncrementalDatatypeParser {

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    return text;
  }

  @Override
  public StringBuilder makeBuilderWithCapacity(int capacity) {
    return new StringBuilder(capacity, TextType.VARIABLE_LENGTH);
  }

  @Override
  public WithAggregatedProblems<Storage<?>> parseColumn(
      String columnName, Storage<String> sourceStorage) {
    return new WithAggregatedProblems<>(sourceStorage, AggregatedProblems.of());
  }
}
