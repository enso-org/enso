package org.enso.table.data.column.operation.aggregate;

import org.enso.base.Polyglot_Utils;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Value;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/** Aggregates the storage using a provided {@link Function}. */
public class FunctionAggregator extends Aggregator {
  private final Function<List<Object>, Value> aggregateFunction;
  private final boolean skipNa;
  private final Storage storage;
  private final InferredBuilder builder;

  /**
   * @param aggregateFunction the function used to obtain aggregation of a group
   * @param storage the storage serving as data source
   * @param skipNa whether missing values should be passed to the function
   * @param resultSize the number of times {@link Aggregator#nextGroup(IntStream)} will be called
   */
  public FunctionAggregator(
      Function<List<Object>, Value> aggregateFunction,
      Storage storage,
      boolean skipNa,
      int resultSize) {
    this.aggregateFunction = aggregateFunction;
    this.storage = storage;
    this.skipNa = skipNa;
    this.builder = new InferredBuilder(resultSize);
  }

  @Override
  public void nextGroup(IntStream positions) {
    List<Object> items = getItems(positions);
    Value result = aggregateFunction.apply(items);
    Object converted = Polyglot_Utils.convertPolyglotValue(result);
    builder.appendNoGrow(converted);
  }

  private List<Object> getItems(IntStream positions) {
    Stream<Object> items = positions.mapToObj(storage::getItemBoxed);
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
