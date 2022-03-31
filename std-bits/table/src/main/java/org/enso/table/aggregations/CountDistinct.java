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
 */
public class CountDistinct extends AggregateColumn {
  private final Storage[] storage;
  private final boolean ignoreEmpty;

  public CountDistinct(String name, Column[] columns, boolean ignoreEmpty) {
    super(name, Storage.Type.LONG);
    this.storage = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    this.ignoreEmpty = ignoreEmpty;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Set<MultiValueKey> set = new HashSet<>();
    for (int row: indexes) {
      MultiValueKey key = new MultiValueKey(Arrays.stream(storage).map(s->s.getItemBoxed(row)).toArray());
      if (key.hasFloatValues()) {
        this.addProblem(new FloatingPointGrouping(this.getName(), row));
      }

      if (!ignoreEmpty || !key.areAllNull()) {
        set.add(key);
      }
    }
    return set.size();
  }
}
