package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/***
 * Aggregate Column computing the most common value in a group (ignoring Nothing).
 */
public class Mode extends AggregateColumn {
  private final Storage storage;

  public Mode(String name, Column column) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    Object current = null;
    int count = 0;
    Map<Object, Integer> currentMap = null;
    for (int row: rows) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        // Merge all numbers onto Double
        Double dValue = CastToDouble(value);
        value = dValue == null ? value : dValue;

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
