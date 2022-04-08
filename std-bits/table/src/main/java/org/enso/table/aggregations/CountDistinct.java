package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.FloatingPointGrouping;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/***
 * Aggregate Column counting the number of distinct items in a group.
 * If `ignoreAllNull` is true, does count when all items are null.
 */
public class CountDistinct extends Aggregator {
  private final Storage[] storage;
  private final boolean ignoreAllNull;

  /**
   * Constructs a CountDistinct Aggregator
   * @param name output column name
   * @param columns input columns
   * @param ignoreAllNull if true ignore then all values are null
   */
  public CountDistinct(String name, Column[] columns, boolean ignoreAllNull) {
    super(name, Storage.Type.LONG);
    this.storage = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    this.ignoreAllNull = ignoreAllNull;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Set<MultiValueKey> set = new HashSet<>();
    for (int row: indexes) {
      MultiValueKey key = new MultiValueKey(Arrays.stream(storage).map(s->s.getItemBoxed(row)).toArray());
      if (key.hasFloatValues()) {
        this.addProblem(new FloatingPointGrouping(this.getName(), row));
      }

      if (!ignoreAllNull || !key.areAllNull()) {
        set.add(key);
      }
    }
    return set.size();
  }
}
