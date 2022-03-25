package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.function.Function;

public class AggregateColumnDefinition {
  @FunctionalInterface
  private interface Aggregator {
    Object aggregate(Object current, int row);
  }

  public static class InvalidAggregation {
    private final String columnName;
    private final int row;
    private final String message;

    public InvalidAggregation(String columnName, int row, String message) {
      this.columnName = columnName;
      this.row = row;
      this.message = message;
    }

    public String getColumnName() {
      return columnName;
    }

    public int getRow() {
      return row;
    }

    public String getMessage() {
      return message;
    }
  }

  private final String name;
  private final int type;
  private final Object initialValue;
  private final Aggregator aggregator;
  private final Function<Object, Object> finalizer;

  private AggregateColumnDefinition(String name, int type, Object initialValue, Aggregator aggregator, Function<Object, Object> finalizer) {
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

  public Object aggregate(Object current, int row) {
    return (current instanceof InvalidAggregation ? current : aggregator.aggregate(current, row));
  }

  public Object finalise(Object current) {
    return (current instanceof InvalidAggregation ? current : finalizer.apply(current));
  }

  public static AggregateColumnDefinition GroupBy(String name, Column column) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(name, Storage.Type.OBJECT, null, (v, row) -> storage.getItemBoxed(row), v -> v);
  }

  public static AggregateColumnDefinition Count(String name) {
    return new AggregateColumnDefinition(name, Storage.Type.LONG, 0L, (v, row) -> (Long)v + 1L, v -> v);
  }

  public static AggregateColumnDefinition CountNothing(String name, Column column, boolean isNothing) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.LONG,
        0L,
        (v, row) -> (Long)v + ((storage.getItemBoxed(row) == null) == isNothing ? 1 : 0),
        v -> v);
  }

  public static AggregateColumnDefinition CountEmpty(String name, Column column, boolean isEmpty) {
    Storage storage = column.getStorage();
    Aggregator aggregator = (v, row) -> {
      Object value = storage.getItemBoxed(row);

      if (value == null || value instanceof String) {
        return (Long)v + ((value == null || ((String)value).length() > 0) == isEmpty ? 1 : 0);
      }

      return new InvalidAggregation(name, row, "Non-Text value - cannot aggregate");
    };

    return new AggregateColumnDefinition(name, Storage.Type.LONG,0L, aggregator, v -> v);
  }
}
