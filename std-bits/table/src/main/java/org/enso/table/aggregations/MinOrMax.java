package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.Comparator;
import java.util.List;

/**
 * Aggregate Column finding the minimum (minOrMax = -1) or maximum (minOrMax = 1) entry in a group.
 */
public class MinOrMax extends Aggregator {
  private final Storage storage;
  private final int minOrMax;
  private final Comparator<Object> objectComparator;

  /**
   * Constructs a MinOrMax Aggregator
   *
   * @param name output column name
   * @param column input column
   * @param minOrMax <0 for minimum, >0 for maximum
   */
  public MinOrMax(String name, Column column, int minOrMax, Comparator<Object> objectComparator) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.minOrMax = Integer.signum(minOrMax);
    this.objectComparator = objectComparator;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Object current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        try {
          if (current == null
              || Integer.signum(objectComparator.compare(value, current)) == minOrMax) {
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
}
