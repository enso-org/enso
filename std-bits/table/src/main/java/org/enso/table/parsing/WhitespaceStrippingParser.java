package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.parsing.problems.ProblemAggregator;

public class WhitespaceStrippingParser<PA extends ProblemAggregator> extends IncrementalDatatypeParser<PA> {
  private final IncrementalDatatypeParser<PA> innerParser;

  public WhitespaceStrippingParser(IncrementalDatatypeParser<PA> innerParser) {
    this.innerParser = innerParser;
  }

  @Override
  public Object parseSingleValue(String text, PA problemAggregator) {
    String stripped = text.strip();
    return innerParser.parseSingleValue(stripped, problemAggregator);
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    return innerParser.makeBuilderWithCapacity(capacity);
  }

  @Override
  public PA makeProblemAggregator() {
    return innerParser.makeProblemAggregator();
  }
}
