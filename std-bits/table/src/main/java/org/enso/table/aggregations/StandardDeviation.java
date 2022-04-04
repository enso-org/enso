package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

/***
 * Aggregate Column computing the standard deviation of a group.
 */
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

  private final Storage storage;
  private final boolean population;

  public StandardDeviation(String name, Column column,boolean population) {
    super(name, Storage.Type.DOUBLE);
    this.storage = column.getStorage();
    this.population = population;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Calculation current = null;
    for (int row: indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        Double dValue = CastToDouble(value);
        if (dValue == null) {
          this.addProblem(new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
          return null;
        }

        if (current == null) {
          current = new Calculation(dValue);
        } else {
          current.count++;
          current.total += dValue;
          current.total_sqr += dValue*dValue;
        }
      }
    }
    return current == null ? null :
        (population ? 1 : Math.sqrt(current.count / (current.count - 1.0))) *
            Math.sqrt(current.total_sqr / current.count - Math.pow(current.total / current.count, 2));
  }
}
