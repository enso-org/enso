package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

/** Aggregate Column computing the total value in a group. */
public class Sum extends Aggregator {
  private final Storage<?> storage;

  public Sum(String name, Column column) {
    super(name, Storage.Type.DOUBLE);
    this.storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Object current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        if (current == null) {
          current = 0L;
        }

        Long lCurrent = CastToLong(current);
        Long lValue = CastToLong(value);
        if (lCurrent != null && lValue != null) {
          current = lCurrent + lValue;
        } else {
          Double dCurrent = CastToDouble(current);
          Double dValue = CastToDouble(value);
          if (dCurrent != null && dValue != null) {
            current = dCurrent + dValue;
          } else {
            this.addProblem(
                new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
            return null;
          }
        }
      }
    }
    return current;
  }
}
