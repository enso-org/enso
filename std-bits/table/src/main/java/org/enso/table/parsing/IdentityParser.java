package org.enso.table.parsing;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
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
  public Builder makeBuilderWithCapacity(long capacity, ProblemAggregator problemAggregator) {
    return Builder.getForText(TextType.VARIABLE_LENGTH, capacity);
  }

  @Override
  public ColumnStorage<?> parseColumn(
      ColumnStorage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
    return sourceStorage;
  }
}
