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

/** Aggregate Column computing the standard deviation of a group. */
public class StandardDeviation extends Aggregator {
  private static class Calculation {
    public long count;
    public double total;
    public double total_sqr;

    public Calculation(double value) {
      count = 1;
      total = value;
      total_sqr = value * value;
    }
  }

  private final Storage<?> storage;
  private final boolean population;
  private final ColumnAggregatedProblemAggregator problemAggregator;

  public StandardDeviation(String name, Column column, boolean population, ProblemAggregator problemAggregator) {
    super(name, FloatType.FLOAT_64);
    this.storage = column.getStorage();
    this.population = population;
    this.problemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Context context = Context.getCurrent();
    Calculation current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        Double dValue = NumericConverter.tryConvertingToDouble(value);
        if (dValue == null) {
          problemAggregator.reportColumnAggregatedProblem(
              new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
          return null;
        }

        if (current == null) {
          current = new Calculation(dValue);
        } else {
          current.count++;
          current.total += dValue;
          current.total_sqr += dValue * dValue;
        }
      }

      context.safepoint();
    }

    if (current == null || (!population && current.count <= 1)) return null;
    return (population ? 1 : Math.sqrt(current.count / (current.count - 1.0)))
        * Math.sqrt(current.total_sqr / current.count - Math.pow(current.total / current.count, 2));
  }
}
