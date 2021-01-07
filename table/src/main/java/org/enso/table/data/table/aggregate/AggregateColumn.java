package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.Index;
import org.enso.table.data.table.Column;

import java.util.List;
import java.util.function.Function;

public class AggregateColumn {
  private final Index uniqueIndex;
  private final Column column;

  public AggregateColumn(Index uniqueIndex, Column column) {
    this.uniqueIndex = uniqueIndex;
    this.column = column;
  }

  public Column aggregate(
      String aggName,
      String outSuffix,
      Function<List<Object>, Object> aggregatorFunction,
      boolean skipNa) {
    Aggregator aggregator =
        column.getStorage().getAggregator(aggName, aggregatorFunction, skipNa, uniqueIndex.size());

    for (int i = 0; i < uniqueIndex.size(); i++) {
      List<Integer> ixes = column.getIndex().loc(uniqueIndex.iloc(i));
      aggregator.nextGroup(ixes);
    }
    return new Column(column.getName() + outSuffix, uniqueIndex, aggregator.seal());
  }

  public Column getColumn() {
    return column;
  }
}
