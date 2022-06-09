package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKey;
import org.enso.table.data.table.Column;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/** Aggregate Column finding the first value in a group. */
public class First extends Aggregator {
  private final Storage storage;
  private final Storage[] ordering;
  private final Comparator<Object> objectComparator;
  private final boolean ignoreNothing;

  public First(String name, Column column, boolean ignoreNothing) {
    this(name, column, ignoreNothing, null, null);
  }

  public First(
      String name,
      Column column,
      boolean ignoreNothing,
      Column[] ordering,
      Comparator<Object> objectComparator) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.ordering =
        ordering == null
            ? new Storage[0]
            : Arrays.stream(ordering).map(Column::getStorage).toArray(Storage[]::new);
    this.objectComparator = objectComparator;
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    if (ordering.length == 0) {
      return firstByRowOrder(indexes);
    } else {
      return firstBySpecifiedOrder(indexes);
    }
  }

  private Object firstBySpecifiedOrder(List<Integer> indexes) {
    MultiValueKey key = null;
    Object current = null;

    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (ignoreNothing && value == null) {
        continue;
      }

      MultiValueKey newKey = new MultiValueKey(this.ordering, row, objectComparator);
      if (key == null || key.compareTo(newKey) > 0) {
        key = newKey;
        current = storage.getItemBoxed(row);
      }
    }

    return current;
  }

  private Object firstByRowOrder(List<Integer> indexes) {
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (!ignoreNothing || value != null) {
        return value;
      }
    }
    return null;
  }
}
