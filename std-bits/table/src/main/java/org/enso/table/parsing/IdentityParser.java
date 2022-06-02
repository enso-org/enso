package org.enso.table.parsing;

import java.util.List;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.problems.WithProblems;

/** A parser that just returns its input. Useful as a fallback. */
public class IdentityParser extends IncrementalDatatypeParser {

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    return text;
  }

  @Override
  public StringBuilder makeBuilderWithCapacity(int capacity) {
    return new StringBuilder(capacity);
  }

  @Override
  public WithProblems<Storage> parseColumn(String columnName, StringStorage sourceStorage) {
    return new WithProblems<>(sourceStorage, List.of());
  }
}
