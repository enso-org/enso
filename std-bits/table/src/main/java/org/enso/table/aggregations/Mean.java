package org.enso.table.aggregations;

import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing the mean value in a group. */
public class Mean extends Aggregator {
  private static class Calculation {
    public long count;
    public double total;

    public Calculation(double value) {
      count = 1;
      total = value;
    }
  }

  private final Storage<?> storage;

  public Mean(String name, Column column) {
    super(name, FloatType.FLOAT_64);
    this.storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    Context context = Context.getCurrent();
    Calculation current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        Double dValue = NumericConverter.tryConvertingToDouble(value);
        if (dValue == null) {
          innerAggregator.reportColumnAggregatedProblem(
              new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
          return null;
        }

        if (current == null) {
          current = new Calculation(dValue);
        } else {
          current.count++;
          current.total += dValue;
        }
      }

      context.safepoint();
    }
    return current == null ? null : current.total / current.count;
  }
}
