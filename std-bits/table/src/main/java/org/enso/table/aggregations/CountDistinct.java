package org.enso.table.aggregations;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;
import org.graalvm.polyglot.Context;

/**
 * Aggregate Column counting the number of distinct items in a group. If `ignoreAllNull` is true,
 * does count when all items are null.
 */
public class CountDistinct extends Aggregator {
  private final Storage<?>[] storage;
  private final List<TextFoldingStrategy> textFoldingStrategy;
  private final boolean ignoreAllNull;

  /**
   * Constructs a CountDistinct Aggregator
   *
   * @param name output column name
   * @param columns input columns
   * @param ignoreAllNull if true ignore then all values are null
   */
  public CountDistinct(String name, Column[] columns, boolean ignoreAllNull) {
    super(name, IntegerType.INT_64);
    this.storage = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    this.ignoreAllNull = ignoreAllNull;
    textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, storage.length);
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    Context context = Context.getCurrent();
    HashSet<UnorderedMultiValueKey> set = new HashSet<>();
    for (int row : indexes) {
      UnorderedMultiValueKey key = new UnorderedMultiValueKey(storage, row, textFoldingStrategy);
      if (key.hasFloatValues()) {
        innerAggregator.reportColumnAggregatedProblem(
            new FloatingPointGrouping(this.getName(), row));
      }

      if (!ignoreAllNull || !key.areAllNull()) {
        set.add(key);
      }

      context.safepoint();
    }
    return set.size();
  }
}
