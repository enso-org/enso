package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.BoolBuilder;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.parsing.problems.InvalidFormatProblemAggregator;

public class BooleanParser extends TypeParser<InvalidFormatProblemAggregator> {

  private final String[] trueValues;
  private final String[] falseValues;

  public BooleanParser(String[] trueValues, String[] falseValues) {
    this.trueValues = trueValues;
    this.falseValues = falseValues;
  }

  @Override
  public Object parseSingleValue(String text, InvalidFormatProblemAggregator problemAggregator) {
    // TODO we may want to use equality checks taking Unicode Normalization into account, to be
    // revised in: https://www.pivotaltracker.com/story/show/182166382
    for (var v : trueValues) {
      if (text.equals(v)) return true;
    }

    for (var v : falseValues) {
      if (text.equals(v)) return false;
    }

    problemAggregator.reportInvalidFormat(text);
    return null;
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    return new BoolBuilder((int) capacity);
  }

  @Override
  public InvalidFormatProblemAggregator makeProblemAggregator() {
    return new InvalidFormatProblemAggregator();
  }
}
