package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.Column;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/** Aggregate Column finding the first value in a group. */
public class First extends Aggregator {
  private final Storage storage;
  private final Storage[] orderByColumns;
  private final int[] orderByDirections;
  private final Comparator<Object> objectComparator;
  private final boolean ignoreNothing;

  public First(String name, Column column, boolean ignoreNothing) {
    this(name, column, ignoreNothing, null, null, null);
  }

  public First(
      String name,
      Column column,
      boolean ignoreNothing,
      Column[] orderByColumns,
      Long[] orderByDirections,
      Comparator<Object> objectComparator) {
    super(name, column.getStorage().getType());
    this.storage = column.getStorage();
    this.orderByColumns =
        orderByColumns == null
            ? new Storage[0]
            : Arrays.stream(orderByColumns).map(Column::getStorage).toArray(Storage[]::new);
    this.orderByDirections =
        orderByDirections == null
            ? new int[0]
            : Arrays.stream(orderByDirections).mapToInt(Long::intValue).toArray();
    this.objectComparator = objectComparator;
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    if (orderByColumns.length == 0) {
      return firstByRowOrder(indexes);
    } else {
      return firstBySpecifiedOrder(indexes);
    }
  }

  private Object firstBySpecifiedOrder(List<Integer> indexes) {
    OrderedMultiValueKey key = null;
    Object current = null;

    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (ignoreNothing && value == null) {
        continue;
      }

      OrderedMultiValueKey newKey =
          new OrderedMultiValueKey(
              this.orderByColumns, row, this.orderByDirections, objectComparator);
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
