package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.*;

/***
 * Aggregate Column computing a percentile value in a group.
 */
public class Percentile extends Aggregator {
  private final Storage storage;
  private final double percentile;

  public Percentile(String name, Column column, double percentile) {
    super(name, Storage.Type.DOUBLE);
    this.storage = column.getStorage();
    this.percentile = percentile;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    int count = 0;
    SortedMap<Double, Integer> currentMap = null;
    for (int row: indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        Double dValue = CastToDouble(value);

        if (dValue == null) {
          this.addProblem(new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
          return null;
        } else if (count == 0) {
          count = 1;
          currentMap = new TreeMap<>();
          currentMap.put(dValue, 1);
        } else {
          count++;
          currentMap.put(dValue, currentMap.getOrDefault(dValue, 0) + 1);
        }
      }
    }

    if (count == 0)  {
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
        return first + (second - first) * (mid_value - mid);
      }

      current = nextCurrent;
    }

    this.addProblem(new InvalidAggregation(this.getName(), -1, "Failed calculating the percentile."));
    return null;
  }
}
