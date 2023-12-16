package org.enso.table.aggregations;

import java.util.List;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column finding the longest or shortest string in a group. */
public class ShortestOrLongest extends Aggregator {
  public static final int SHORTEST = -1;
  public static final int LONGEST = 1;
  private final Storage<?> storage;
  private final int minOrMax;

  public ShortestOrLongest(String name, Column column, int minOrMax) {
    super(name, TextType.VARIABLE_LENGTH);
    this.storage = column.getStorage();
    this.minOrMax = minOrMax;
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    Context context = Context.getCurrent();
    long length = 0;
    Object current = null;

    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        if (!(value instanceof String asString)) {
          innerAggregator.reportColumnAggregatedProblem(
              new InvalidAggregation(this.getName(), row, "Not a text value."));
          return null;
        }

        long valueLength = Text_Utils.grapheme_length(asString);
        if (current == null || Long.compare(valueLength, length) == minOrMax) {
          length = valueLength;
          current = value;
        }
      }

      context.safepoint();
    }

    return current;
  }
}
