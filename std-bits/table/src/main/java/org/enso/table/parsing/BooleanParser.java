package org.enso.table.parsing;

import java.util.HashSet;
import java.util.Set;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class BooleanParser extends IncrementalDatatypeParser {

  private final Set<String> trueValues;
  private final Set<String> falseValues;

  public BooleanParser(String[] trueValues, String[] falseValues) {
    this.trueValues = new HashSet<>(trueValues.length);
    this.falseValues = new HashSet<>(falseValues.length);
    for (String v : trueValues) {
      this.trueValues.add(v);
    }
    for (String v : falseValues) {
      this.falseValues.add(v);
    }
  }

  @Override
  public Object parseSingleValue(String text, ParseProblemAggregator problemAggregator) {
    // TODO we may want to use equality checks taking Unicode Normalization into account, to be
    // revised in: https://www.pivotaltracker.com/story/show/182166382
    if (trueValues.contains(text)) return true;
    if (falseValues.contains(text)) return false;

    problemAggregator.reportInvalidFormat(text);
    return null;
  }

  @Override
  protected Builder makeBuilderWithCapacity(long capacity, ProblemAggregator problemAggregator) {
    return Builder.getForBoolean(capacity);
  }
}
