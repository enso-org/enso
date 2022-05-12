package org.enso.table.read.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.read.parsing.problems.ProblemAggregator;

public class WhitespaceStrippingParser<PA extends ProblemAggregator> extends TypeParser<PA> {
  private final TypeParser<PA> innerParser;

  public WhitespaceStrippingParser(TypeParser<PA> innerParser) {
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
