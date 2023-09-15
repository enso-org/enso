package org.enso.table.parsing;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.graalvm.collections.EconomicSet;

public class BooleanParser extends IncrementalDatatypeParser {

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
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    // TODO we may want to use equality checks taking Unicode Normalization into account, to be
    // revised in: https://www.pivotaltracker.com/story/show/182166382
    if (trueValues.contains(text)) return true;
    if (falseValues.contains(text)) return false;

    problemAggregator.reportInvalidFormat(text);
    return null;
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity) {
    return new BoolBuilder(capacity);
  }
}
