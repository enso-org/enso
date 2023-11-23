package org.enso.table.aggregations;

import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing the total value in a group. */
public class Sum extends Aggregator {
  private final Storage<?> storage;

  public Sum(String name, Column column) {
    super(name, FloatType.FLOAT_64);
    this.storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    MapOperationProblemAggregator innerAggregator =
        new MapOperationProblemAggregator(problemAggregator, getName());
    Context context = Context.getCurrent();
    Object current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        if (current == null) {
          current = 0L;
        }

        Long lCurrent = NumericConverter.tryConvertingToLong(current);
        Long lValue = NumericConverter.tryConvertingToLong(value);
        if (lCurrent != null && lValue != null) {
          try {
            current = Math.addExact(lCurrent, lValue);
          } catch (ArithmeticException exception) {
            innerAggregator.reportOverflow(IntegerType.INT_64, "Sum");
            return null;
          }
        } else {
          Double dCurrent = NumericConverter.tryConvertingToDouble(current);
          Double dValue = NumericConverter.tryConvertingToDouble(value);
          if (dCurrent != null && dValue != null) {
            current = dCurrent + dValue;
          } else {
            innerAggregator.reportColumnAggregatedProblem(
                new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
            return null;
          }
        }
      }

      context.safepoint();
    }
    return current;
  }
}
