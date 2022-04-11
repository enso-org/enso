package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

/***
 * Aggregate Column finding the minimum (minOrMax = -1) or maximum (minOrMax = 1) entry in a group.
 */
public class MinOrMax extends Aggregator {
  private final Storage storage;
  private final int minOrMax;

  /**
   * Constructs a MinOrMax Aggregator
   * @param name output column name
   * @param column input column
   * @param minOrMax <0 for minimum, >0 for maximum
   */
  public MinOrMax(String name, Column column, int minOrMax) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.minOrMax = Integer.signum(minOrMax);
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Object current = null;
    for (int row: indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        try {
          if (current == null || Integer.signum(Compare(current, value)) == minOrMax) {
            current = value;
          }
        } catch (ClassCastException e) {
          this.addProblem(new InvalidAggregation(this.getName(), row, "Cannot compare values."));
          return null;
        }
      }
    }
    return current;
  }

  private static int Compare(Object current, Object value) {
    if (current instanceof String && value instanceof String) {
      return ((String)value).compareTo((String)current);
    }

    if (current instanceof Long) {
      Long lValue = CastToLong(value);
      if (null != lValue) {
        return Long.compare(lValue, (Long)current);
      }

      Double dValue = CastToDouble(value);
      if (null != dValue) {
        return Double.compare(dValue, (Long)current);
      }
    }

    if (current instanceof Double) {
      Double dValue = CastToDouble(value);
      if (null != dValue) {
        return Double.compare(dValue, (Double)current);
      }
    }

    throw new ClassCastException();
  }
}
