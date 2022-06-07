package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKey;
import org.enso.table.data.table.Column;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class Last extends Aggregator {
  private final Storage storage;
  private final Storage[] ordering;
  private final Comparator<Object> objectComparator;
  private final boolean ignoreNothing;

  public Last(String name, Column column, boolean ignoreNothing) {
    this(name, column, ignoreNothing, null, null);
  }

  public Last(
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
      return lastByRowOrder(indexes);
    } else {
      return lastBySpecifiedOrder(indexes);
    }
  }

  private Object lastBySpecifiedOrder(List<Integer> indexes) {
    MultiValueKey key = null;
    Object current = null;

    for (int i = indexes.size() - 1; i >= 0; i--) {
      int row = indexes.get(i);
      Object value = storage.getItemBoxed(row);
      if (ignoreNothing && value == null) {
        continue;
      }

      MultiValueKey newKey = new MultiValueKey(this.ordering, row, objectComparator);
      if (key == null || key.compareTo(newKey) < 0) {
        key = newKey;
        current = storage.getItemBoxed(row);
      }
    }

    return current;
  }

  private Object lastByRowOrder(List<Integer> indexes) {
    for (int i = indexes.size() - 1; i >= 0; i--) {
      Object value = storage.getItemBoxed(indexes.get(i));
      if (!ignoreNothing || value != null) {
        return value;
      }
    }
    return null;
  }
}
