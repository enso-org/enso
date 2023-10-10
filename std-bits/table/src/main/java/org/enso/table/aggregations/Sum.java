package org.enso.table.aggregations;

import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
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
  private final MapOperationProblemBuilder problemAggregator;

  public Sum(String name, Column column, ProblemAggregator problemAggregator) {
    super(name, FloatType.FLOAT_64);
    this.storage = column.getStorage();
    this.problemAggregator = new MapOperationProblemBuilder(problemAggregator, name);
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
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
            problemAggregator.reportOverflow(IntegerType.INT_64, "Sum");
            return null;
          }
        } else {
          Double dCurrent = NumericConverter.tryConvertingToDouble(current);
          Double dValue = NumericConverter.tryConvertingToDouble(value);
          if (dCurrent != null && dValue != null) {
            current = dCurrent + dValue;
          } else {
            problemAggregator.reportColumnAggregatedProblem(
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
