package org.enso.table.data.column.operation.aggregate;

import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.storage.Storage;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FunctionAggregator extends Aggregator {
  private final Function<List<Object>, Object> aggregateFunction;
  private final boolean skipNa;
  private final Storage storage;
  private final InferredBuilder builder;

  public FunctionAggregator(
      Function<List<Object>, Object> aggregateFunction,
      Storage storage,
      boolean skipNa,
      int resultSize) {
    this.aggregateFunction = aggregateFunction;
    this.storage = storage;
    this.skipNa = skipNa;
    this.builder = new InferredBuilder(resultSize);
  }

  @Override
  public void nextGroup(List<Integer> positions) {
    List<Object> items = getItems(positions);
    Object result = aggregateFunction.apply(items);
    builder.append(result);
  }

  private List<Object> getItems(List<Integer> positions) {
    Stream<Object> items = positions.stream().map(storage::getItemBoxed);
    if (skipNa) {
      items = items.filter(Objects::nonNull);
    }
    return items.collect(Collectors.toList());
  }

  @Override
  public Storage seal() {
    return builder.seal();
  }
}
