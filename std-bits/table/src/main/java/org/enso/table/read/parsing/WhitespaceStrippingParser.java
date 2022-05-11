package org.enso.table.read.parsing;

import org.enso.table.data.column.builder.object.Builder;

public class WhitespaceStrippingParser extends TypeParser {
  private final TypeParser innerParser;

  public WhitespaceStrippingParser(TypeParser innerParser) {
    this.innerParser = innerParser;
  }


  @Override
  public Object parseSingleValue(String text) {
    String stripped = text.strip();
    return innerParser.parseSingleValue(stripped);
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    return innerParser.makeBuilderWithCapacity(capacity);
  }
}
