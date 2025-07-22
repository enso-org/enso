package org.enso.table.operations;

import java.util.Comparator;
import java.util.stream.LongStream;
import org.enso.base.ObjectComparator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.table.Column;

/** Builds an order mask resulting in sorting storages according to specified rules. */
public class OrderBuilder {
  public static class OrderRule {
    private final Column column;
    private final boolean ascending;
    private final boolean missingLast;

    /**
     * A single-column ordering rule.
     *
     * @param column the column to use for ordering
     * @param ascending whether column should be sorted ascending or descending
     * @param missingLast whether or not missing values should be placed at the start or end of the
     *     ordering
     */
    public OrderRule(Column column, boolean ascending, boolean missingLast) {
      this.column = column;
      this.ascending = ascending;
      this.missingLast = missingLast;
    }

    /**
     * Builds an index-comparing comparator, that will sort array indexes according to the specified
     * ordering of the underlying column.
     *
     * @return a comparator with properties described above
     */
    public Comparator<Long> toComparator() {
      final ColumnStorage<?> storage = column.getStorage();
      Comparator<Object> itemCmp = ObjectComparator.DEFAULT;

      if (!ascending) {
        itemCmp = itemCmp.reversed();
      }

      if (missingLast) {
        itemCmp = Comparator.nullsLast(itemCmp);
      } else {
        itemCmp = Comparator.nullsFirst(itemCmp);
      }

      final Comparator<Object> cmp = itemCmp;
      return (i, j) -> cmp.compare(storage.getItemBoxed(i), storage.getItemBoxed(j));
    }
  }

  /**
   * Builds an order mask based on the specified set of rules.
   *
   * @param rule a rule that should be used in generating the ordering.
   * @return an order mask that will result in sorting any storage according to the specified rules.
   */
  public static long[] buildMask(OrderRule rule) {
    long size = rule.column.getSize();
    Comparator<Long> comparator = rule.toComparator();
    return LongStream.range(0, size).boxed().sorted(comparator).mapToLong(i -> i).toArray();
  }
}
