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
    Aggregator aggregator = column.getStorage().getAggregator(aggName, aggregatorFunction, )
    InferredBuilder builder = new InferredBuilder(uniqueIndex.size());
    for (int i = 0; i < uniqueIndex.size(); i++) {
      Object r = init;
      List<Integer> ixes = column.getIndex().loc(uniqueIndex.iloc(i));
      Storage st = column.getStorage();
      if (ixes != null) {
        for (int ix : ixes) {
          Object arg = st.getItemBoxed(ix);
          if (r == null && skipNa) {
            r = arg;
          } else if (arg != null || !skipNa) {
            r = aggregator.apply(r, arg);
          }
        }
        builder.append(r);
      }
    }
    return new Column(column.getName() + outSuffix, uniqueIndex, builder.seal());
  }

  public Column getColumn() {
    return column;
  }
}
