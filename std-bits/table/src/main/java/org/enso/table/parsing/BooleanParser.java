package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.BoolBuilder;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.parsing.problems.InvalidFormatProblemAggregator;
import org.graalvm.collections.EconomicSet;

public class BooleanParser extends DatatypeParser<InvalidFormatProblemAggregator> {

  private final EconomicSet<String> trueValues;
  private final EconomicSet<String> falseValues;

  public BooleanParser(String[] trueValues, String[] falseValues) {
    this.trueValues = EconomicSet.create(trueValues.length);
    this.falseValues = EconomicSet.create(falseValues.length);
    for (String v : trueValues) {
      this.trueValues.add(v);
    }
    for (String v : falseValues) {
      this.falseValues.add(v);
    }
  }

  @Override
  public Object parseSingleValue(String text, InvalidFormatProblemAggregator problemAggregator) {
    // TODO we may want to use equality checks taking Unicode Normalization into account, to be
    // revised in: https://www.pivotaltracker.com/story/show/182166382
    if (trueValues.contains(text)) return true;
    if (falseValues.contains(text)) return false;

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
