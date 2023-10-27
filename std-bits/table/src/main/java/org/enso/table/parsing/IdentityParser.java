package org.enso.table.parsing;

import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

/** A parser that just returns its input. Useful as a fallback. */
public class IdentityParser extends IncrementalDatatypeParser {

  @Override
  public Object parseSingleValue(String text, ParseProblemAggregator problemAggregator) {
    return text;
  }

  @Override
  public StringBuilder makeBuilderWithCapacity(int capacity, ProblemAggregator problemAggregator) {
    return new StringBuilder(capacity, TextType.VARIABLE_LENGTH);
  }

  @Override
  public Storage<?> parseColumn(
      Storage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
    return sourceStorage;
  }
}
