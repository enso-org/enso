package org.enso.database.fetchers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.problems.ProblemAggregator;

class InferredColumnFetcher extends BaseColumnFetcher {
  InferredColumnFetcher(int index, String name, ProblemAggregator problemAggregator) {
    super(index, name, Builder.getInferredBuilder(DEFAULT_SIZE, problemAggregator));
  }
}
