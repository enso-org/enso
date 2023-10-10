package org.enso.table.aggregations;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing the most common value in a group (ignoring Nothing). */
public class Mode extends Aggregator {
  private final Storage<?> storage;
  private final ColumnAggregatedProblemAggregator problemAggregator;

  public Mode(String name, Column column, ProblemAggregator problemAggregator) {
    super(name, column.getStorage().getType());
    this.storage = column.getStorage();
    this.problemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Context context = Context.getCurrent();
    Object current = null;
    int count = 0;
    Map<Object, Integer> currentMap = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        // Merge all numbers onto a Long if possible or a Double if needed
        Long lValue = NumericConverter.tryConvertingToLong(value);
        if (lValue == null) {
          Double dValue = NumericConverter.tryConvertingToDouble(value);
          if (dValue != null) {
            problemAggregator.reportColumnAggregatedProblem(new FloatingPointGrouping(this.getName(), row));
            value = dValue;
          }
        } else {
          value = lValue;
        }

        if (current == null) {
          current = value;
          count = 1;
          currentMap = new HashMap<>();
          currentMap.put(value, 1);
        } else {
          int newCount = currentMap.getOrDefault(value, 0) + 1;
          currentMap.put(value, newCount);
          if (newCount > count) {
            count = newCount;
            current = value;
          }
        }
      }

      context.safepoint();
    }
    return current;
  }
}
