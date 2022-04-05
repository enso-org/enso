package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKey;
import org.enso.table.data.table.Column;

import java.util.Arrays;
import java.util.List;

/***
 * Aggregate Column finding the first value in a group.
 */
public class First extends Aggregator {
  private final Storage storage;
  private final Storage[] ordering;
  private final boolean ignoreNothing;

  public First(String name, Column column, boolean ignoreNothing) {
    this(name, column, new Column[0], ignoreNothing);
  }

  public First(String name, Column column, Column[] ordering, boolean ignoreNothing) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.ordering = ordering == null ? new Storage[0] : Arrays.stream(ordering).map(Column::getStorage).toArray(Storage[]::new);
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    if (ordering.length == 0) {
      for (int row : indexes) {
        Object value = storage.getItemBoxed(row);
        if (!ignoreNothing || value != null) {
          return value;
        }
      }
    } else {
      MultiValueKey key = null;
      Object current = null;

      for (int row : indexes) {
        Object value = storage.getItemBoxed(row);
        if (ignoreNothing && value == null) {
          continue;
        }

        MultiValueKey newKey = new MultiValueKey(Arrays.stream(this.ordering).map(o -> o.getItemBoxed(row)).toArray());
        if (key == null || key.compareTo(newKey) > 0) {
          key = newKey;
          current = storage.getItemBoxed(row);
        }
      }

      return current;
    }
    return null;
  }
}
