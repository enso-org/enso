package org.enso.table.parsing;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.parsing.problems.ProblemAggregator;

/**
 * An incremental parser which wraps another parser of that type, delegating the parsing logic to
 * it, but first transforming the input text by stripping any leading and trailing whitespace.
 */
public class WhitespaceStrippingParser extends IncrementalDatatypeParser {
  private final IncrementalDatatypeParser innerParser;

  public WhitespaceStrippingParser(IncrementalDatatypeParser innerParser) {
    this.innerParser = innerParser;
  }

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    String stripped = text.strip();
    return innerParser.parseSingleValue(stripped, problemAggregator);
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return innerParser.makeBuilderWithCapacity(capacity);
  }
}
