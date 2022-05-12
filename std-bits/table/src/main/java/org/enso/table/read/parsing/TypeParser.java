package org.enso.table.read.parsing;

import java.util.ArrayList;
import java.util.List;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.read.WithProblems;
import org.enso.table.read.parsing.problems.ProblemAggregator;

public abstract class TypeParser<PA extends ProblemAggregator> {
  public abstract Object parseSingleValue(String text, PA problemAggregator);

  public abstract Builder makeBuilderWithCapacity(long capacity);

  public abstract PA makeProblemAggregator();

  public WithProblems<Storage> parseColumn(StringStorage sourceStorage) {
    List<String> invalidCells = new ArrayList<>();
    Builder builder = makeBuilderWithCapacity(sourceStorage.size());
    PA aggregator = makeProblemAggregator();

    for (int i = 0; i < sourceStorage.size(); ++i) {
      String cell = sourceStorage.getItem(i);
      var parsed = parseSingleValue(cell, aggregator);
      builder.appendNoGrow(parsed);
    }

    return new WithProblems<>(builder.seal(), aggregator.getAggregatedProblems());
  }
}
