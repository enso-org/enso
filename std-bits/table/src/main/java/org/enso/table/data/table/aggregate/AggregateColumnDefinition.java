package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import com.ibm.icu.text.BreakIterator;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class AggregateColumnDefinition {
  @FunctionalInterface
  private interface Aggregator {
    Object aggregate(List<Integer> rows);
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
  private final Aggregator aggregator;

  private AggregateColumnDefinition(String name, int type, Aggregator aggregator) {
    this.name = name;
    this.type = type;
    this.aggregator = aggregator;
  }

  public String getName() {
    return name;
  }

  public int getType() {
    return type;
  }

  public Object aggregate(int[] rows) {
    return this.aggregate(IntStream.of(rows).boxed().collect(Collectors.toList()));
  }

  public Object aggregate(List<Integer> rows) {
    return this.aggregator.aggregate(rows);
  }

  private static long GraphemeLength(String text) {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(text);

    int count = 0;
    for (int end = iter.next(); end != BreakIterator.DONE; end = iter.next()) {
      count++;
    }

    return count;
  }

  private static int Compare(Object current, Object value) {
    if (current instanceof String && value instanceof String) {
      return ((String)value).compareTo((String)current);
    }

    if (current instanceof Long) {
    }

    if (current instanceof Double) {

    }

    throw new ClassCastException();
  }

  public static AggregateColumnDefinition GroupBy(String name, Column column) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.OBJECT,
        (rows) -> rows.isEmpty() ? null : storage.getItemBoxed(rows.get(0)));
  }

  public static AggregateColumnDefinition Count(String name) {
    return new AggregateColumnDefinition(name, Storage.Type.LONG, List::size);
  }

  public static AggregateColumnDefinition CountNothing(String name, Column column, boolean isNothing) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.LONG,
        rows -> {
            int count = 0;
            for (int row: rows) {
              count += ((storage.getItemBoxed(row) == null) == isNothing ? 1 : 0);
            }
            return count;
        });
  }

  public static AggregateColumnDefinition CountEmpty(String name, Column column, boolean isEmpty) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.LONG,
        rows -> {
          int count = 0;
          for (int row: rows) {
            Object value = storage.getItemBoxed(row);
            if (value != null && !(value instanceof String)) {
              return new InvalidAggregation(name, row, "Non-Text value - cannot Count " + (isEmpty ? "Empty" : "Non-Empty"));
            }

            count += ((value == null || ((String)value).length() == 0) == isEmpty ? 1 : 0);
          }
          return count;
        });
  }

  public static AggregateColumnDefinition MinOrMax(String name, Column column, int minOrMax) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.OBJECT,
        rows -> {
          Object current = null;
          for (int row: rows) {
            Object value = storage.getItemBoxed(row);
            if (value != null) {
              try {
                if (current == null || Compare(current, value) == minOrMax) {
                  current = value;
                }
              } catch (ClassCastException e) {
                return new InvalidAggregation(name, row, "Cannot Compare Values.");
              }
            }
          }
          return current;
        });
  }

  public static AggregateColumnDefinition MinOrMaxLength(String name, Column column, int minOrMax) {
    Storage storage = column.getStorage();

    return new AggregateColumnDefinition(
        name,
        Storage.Type.STRING,
        rows -> {
          long length = 0;
          Object current = null;

          for (int row: rows) {
            Object value = storage.getItemBoxed(row);
            if (value != null) {
              if (!(value instanceof String)) {
                return new InvalidAggregation(name, row, "Non-Text value - cannot find " + (minOrMax == 1 ? "Longest" : "Shortest"));
              }

              long valueLength = GraphemeLength((String)value);
              if (current == null || Long.compare(valueLength, length) == minOrMax) {
                length = valueLength;
                current = value;
              }
            }
          }

          return current;
        });
  }

  public static AggregateColumnDefinition First(String name, Column column, boolean ignoreNothing) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.OBJECT,
        rows -> {
          for (int row: rows) {
            Object value = storage.getItemBoxed(row);
            if (!ignoreNothing || value != null) {
              return value;
            }
          }
          return null;
        });
  }

  public static AggregateColumnDefinition Last(String name, Column column, boolean ignoreNothing) {
    Storage storage = column.getStorage();
    return new AggregateColumnDefinition(
        name,
        Storage.Type.OBJECT,
        rows -> {
          Object current = null;
          for (int row: rows) {
            Object value = storage.getItemBoxed(row);
            if (!ignoreNothing || value != null) {
              current = value;
            }
          }
          return current;
        });
  }
}
