package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Table;

import java.util.function.Function;

public class AggregateColumnDefinition {
  @FunctionalInterface
  public interface Aggregator {
    Object aggregate(Object current, Table table, int row);
  }

  private final String name;
  private final int type;
  private final Object initialValue;
  private final Aggregator aggregator;
  private final Function<Object, Object> finalizer;

  public AggregateColumnDefinition(String name, int type, Object initialValue, Aggregator aggregator, Function<Object, Object> finalizer) {
    this.name = name;
    this.type = type;
    this.initialValue = initialValue;
    this.aggregator = aggregator;
    this.finalizer = finalizer;
  }

  public String getName() {
    return name;
  }

  public int getType() {
    return type;
  }

  public Object getInitialValue() {
    return initialValue;
  }

  public Object aggregate(Object current, Table table, int row) {
    return aggregator.aggregate(current, table, row);
  }

  public Object finalise(Object current) {
    return finalizer.apply(current);
  }

  public static AggregateColumnDefinition Count(String name) {
    return new AggregateColumnDefinition(name, Storage.Type.LONG, 0L, (v, table, row) -> (Long)v + 1L, v -> v);
  }
}
