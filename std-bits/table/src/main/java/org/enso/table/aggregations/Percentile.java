package org.enso.table.aggregations;

import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** Aggregate Column computing a percentile value in a group. */
public class Percentile extends Aggregator {
  private final Storage<?> storage;
  private final double percentile;

  public Percentile(String name, Column column, double percentile) {
    super(name, FloatType.FLOAT_64);
    this.storage = column.getStorage();
    this.percentile = percentile;
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    Context context = Context.getCurrent();
    int count = 0;
    SortedMap<Double, Integer> currentMap = new TreeMap<>();
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        Double dValue = NumericConverter.tryConvertingToDouble(value);

        if (dValue == null) {
          innerAggregator.reportColumnAggregatedProblem(
              new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
          return null;
        } else if (dValue.isNaN()) {
          // If any of the input values is a NaN, we do not know where in the ordering it should be
          // and so we return NaN.
          return Double.NaN;
        } else {
          count++;
          currentMap.put(dValue, currentMap.getOrDefault(dValue, 0) + 1);
        }
      }

      context.safepoint();
    }

    if (count == 0) {
      return null;
    }

    double mid_value = (count - 1) * percentile + 1;
    if (mid_value <= 1) {
      return currentMap.firstKey();
    } else if (mid_value >= count) {
      return currentMap.lastKey();
    }

    double mid = Math.floor(mid_value);

    double first = 0;
    int current = 0;
    for (Map.Entry<Double, Integer> entry : currentMap.entrySet()) {
      int nextCurrent = current + entry.getValue();

      if (current <= mid - 1 && nextCurrent > mid - 1) {
        first = entry.getKey();
      }

      if (current <= mid && nextCurrent > mid) {
        double second = entry.getKey();
        return interpolate(first, second, mid_value - mid);
      }

      current = nextCurrent;
      context.safepoint();
    }

    innerAggregator.reportColumnAggregatedProblem(
        new InvalidAggregation(this.getName(), -1, "Failed calculating the percentile."));
    return null;
  }

  double interpolate(double first, double second, double alpha) {
    if (Double.isInfinite(first) && Double.isInfinite(second)) {
      if (first == second) return first;
      else return Double.NaN;
    }

    // If both are not infinite, then if one of them is infinite, the other must be finite.
    if (Double.isInfinite(first)) return first;
    if (Double.isInfinite(second)) return second;

    return first + (second - first) * alpha;
  }
}
