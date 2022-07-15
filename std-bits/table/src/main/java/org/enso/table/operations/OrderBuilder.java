package org.enso.table.operations;

import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.table.Column;

/** Builds an order mask resulting in sorting storages according to specified rules. */
public class OrderBuilder {
  public static class OrderRule {
    private final Column column;
    private final Comparator<Object> customComparator;
    private final boolean ascending;
    private final boolean missingLast;

    /**
     * A single-column ordering rule.
     *
     * @param column the column to use for ordering
     * @param customComparator a comparator that should be used instead of natural ordering of the
     *     values
     * @param ascending whether column should be sorted ascending or descending
     * @param missingLast whether or not missing values should be placed at the start or end of the
     *     ordering
     */
    public OrderRule(
        Column column,
        Comparator<Object> customComparator,
        boolean ascending,
        boolean missingLast) {
      this.column = column;
      this.customComparator = customComparator;
      this.ascending = ascending;
      this.missingLast = missingLast;
    }

    /**
     * Builds an index-comparing comparator, that will sort array indexes according to the specified
     * ordering of the underlying column.
     *
     * @return a comparator with properties described above
     */
    public Comparator<Integer> toComparator() {
      final Storage storage = column.getStorage();
      Comparator<Object> itemCmp = customComparator;
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
   * @param rules a list of rules that should be used in generating the ordering. The rules are
   *     treated hierarchically, i.e. the first rule is applied first, all the groups of equal
   *     elements are then internally reordered according to the second rule etc. The ordering is
   *     stable, i.e. if no rule disambiguates the ordering, the original position in the storage is
   *     used instead.
   * @return an order mask that will result in sorting any storage according to the specified rules.
   */
  public static OrderMask buildOrderMask(List<OrderRule> rules) {
    int size = rules.get(0).column.getSize();
    Comparator<Integer> comparator =
        rules.stream().map(OrderRule::toComparator).reduce(Comparator::thenComparing).get();

    int[] positions =
        IntStream.range(0, size).boxed().sorted(comparator).mapToInt(i -> i).toArray();
    return new OrderMask(positions);
  }

  /**
   * Builds an order mask based that will reverse the order of the data being masked.
   *
   * @param size the size of the data being masked
   * @return an order mask that will result in reversing the data it is applied to
   */
  public static OrderMask buildReversedMask(int size) {
    int[] positions = IntStream.range(0, size).map(i -> size - i - 1).toArray();
    return new OrderMask(positions);
  }
}
