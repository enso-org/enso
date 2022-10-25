package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.FloatingPointGrouping;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Aggregate Column computing the most common value in a group (ignoring Nothing). */
public class Mode extends Aggregator {
  private final Storage<?> storage;

  public Mode(String name, Column column) {
    super(name, column.getStorage().getType());
    this.storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Object current = null;
    int count = 0;
    Map<Object, Integer> currentMap = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        // Merge all numbers onto a Long if possible or a Double if needed
        Long lValue = CastToLong(value);
        if (lValue == null) {
          Double dValue = CastToDouble(value);
          if (dValue != null) {
            this.addProblem(new FloatingPointGrouping(this.getName(), row));
            value = dValue;
          }
        } else {
          value = lValue;
        }

        if (current == null) {
          current = value;
          count = 1;
          currentMap = new HashMap<>();
          currentMap.put(value, 1);
        } else {
          int newCount = currentMap.getOrDefault(value, 0) + 1;
          currentMap.put(value, newCount);
          if (newCount > count) {
            count = newCount;
            current = value;
          }
        }
      }
    }
    return current;
  }
}
