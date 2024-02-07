package org.enso.table.data.table;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.Text_Utils;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.index.CrossTabIndex;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.Index;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.MultiValueKeyBase;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.data.table.join.CrossJoin;
import org.enso.table.data.table.join.JoinKind;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.conditions.JoinCondition;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.operations.Distinct;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;

/** A representation of a table structure. */
public class Table {

  private final Column[] columns;

  /**
   * Creates a new table
   *
   * @param columns the columns contained in this table.
   */
  public Table(Column[] columns) {
    if (columns.length == 0) {
      throw new IllegalArgumentException("A Table must have at least one column.");
    }

    if (!checkUniqueColumns(columns)) {
      throw new IllegalArgumentException("Column names must be unique within a Table.");
    }

    this.columns = columns;
  }

  private static boolean checkUniqueColumns(Column[] columns) {
    HashSet<String> names = new HashSet<>();
    for (Column column : columns) {
      boolean wasNew = names.add(column.getName());
      if (!wasNew) {
        return false;
      }
    }

    return true;
  }

  /**
   * @return the number of rows in this table
   */
  public int rowCount() {
    return columns[0].getSize();
  }

  /**
   * @return the columns of this table
   */
  public Column[] getColumns() {
    return columns;
  }

  /**
   * Returns a column with the given name, or null if it doesn't exist.
   *
   * @param name the column name
   * @return a column with the given name
   */
  public Column getColumnByName(String name) {
    for (Column column : columns) {
      if (Text_Utils.equals(column.getName(), name)) {
        return column;
      }
    }
    return null;
  }

  /**
   * Returns a table resulting from selecting only the rows corresponding to true entries in the
   * provided column.
   *
   * @param maskCol the masking column
   * @return the result of masking this table with the provided column
   */
  public Table mask(Column maskCol) {
    if (!(maskCol.getStorage() instanceof BoolStorage storage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }

    var mask = BoolStorage.toMask(storage);
    var localStorageMask = new BitSet();
    localStorageMask.set(0, rowCount());
    mask.and(localStorageMask);
    int cardinality = mask.cardinality();
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(mask, cardinality);
    }
    return new Table(newColumns);
  }

  /**
   * Adds a column, or replaces it, by name.
   *
   * @param newColumn the column to include.
   * @return a new table containing the specified column.
   */
  public Table addOrReplaceColumn(Column newColumn) {
    int existingIx = -1;
    for (int i = 0; i < columns.length; i++) {
      if (Text_Utils.equals(columns[i].getName(), newColumn.getName())) {
        existingIx = i;
        break;
      }
    }
    if (existingIx == -1) {
      return addColumn(newColumn);
    } else {
      return replaceColumn(existingIx, newColumn);
    }
  }

  private Table replaceColumn(int ix, Column newCol) {
    Column[] newCols = new Column[columns.length];
    System.arraycopy(columns, 0, newCols, 0, columns.length);
    newCols[ix] = newCol;
    return new Table(newCols);
  }

  private Table addColumn(Column newColumn) {
    Column[] newCols = new Column[columns.length + 1];
    System.arraycopy(columns, 0, newCols, 0, columns.length);
    newCols[columns.length] = newColumn;
    return new Table(newCols);
  }

  /**
   * Returns the index of this table.
   *
   * @return the index of this table
   */
  public Index getIndex() {
    return new DefaultIndex(rowCount());
  }

  /**
   * Creates an index for this table by using values from the specified columns.
   *
   * @param columns set of columns to use as an Index
   * @return a table indexed by the proper column
   */
  public MultiValueIndex<?> indexFromColumns(
      Column[] columns, ProblemAggregator problemAggregator) {
    return MultiValueIndex.makeUnorderedIndex(
        columns, this.rowCount(), TextFoldingStrategy.unicodeNormalizedFold, problemAggregator);
  }

  /**
   * Build a cross-tab table on the given grouping and naming columns, aggregating across the
   * aggregate columns.
   *
   * @param groupingColumns specifies the rows of the cross-tab table
   * @param nameColumn specifies the values of the columns of the cross-tab table
   * @param aggregates the columns to aggregate across rows and columns
   * @param aggregateNames the names of the aggregate columns
   * @param problemAggregator an aggregator for problems
   * @return a cross-tab table
   */
  public Table makeCrossTabTable(
      Column[] groupingColumns,
      Column nameColumn,
      Aggregator[] aggregates,
      String[] aggregateNames,
      ProblemAggregator problemAggregator) {
    CrossTabIndex index =
        new CrossTabIndex(
            new Column[] {nameColumn}, groupingColumns, this.rowCount(), problemAggregator);
    return index.makeCrossTabTable(aggregates, aggregateNames);
  }

  /**
   * Creates a new table with the rows sorted
   *
   * @param columns set of columns to use as an Index
   * @param objectComparator Object comparator allowing calling back to `compare_to` when needed.
   * @return a table indexed by the proper column
   */
  public Table orderBy(Column[] columns, Long[] directions, Comparator<Object> objectComparator) {
    int[] directionInts = Arrays.stream(directions).mapToInt(Long::intValue).toArray();
    int n = rowCount();
    Context context = Context.getCurrent();
    final Storage<?>[] storages =
        Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    OrderedMultiValueKey[] keys = new OrderedMultiValueKey[n];
    for (int i = 0; i < n; i++) {
      keys[i] = new OrderedMultiValueKey(storages, i, directionInts, objectComparator);
      context.safepoint();
    }
    Arrays.sort(keys);
    OrderMask mask = OrderMask.fromObjects(keys, MultiValueKeyBase::getRowIndex);
    return this.applyMask(mask);
  }

  /**
   * Creates a new table keeping only rows with distinct key columns.
   *
   * @param keyColumns set of columns to use as an Index
   * @param textFoldingStrategy a strategy for folding text columns
   * @param problemAggregator an aggregator for problems
   * @return a table where duplicate rows with the same key are removed
   */
  public Table distinct(
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      ProblemAggregator problemAggregator) {
    var rowsToKeep =
        Distinct.buildDistinctRowsMask(
            rowCount(), keyColumns, textFoldingStrategy, problemAggregator);
    int cardinality = rowsToKeep.cardinality();
    Column[] newColumns = new Column[this.columns.length];
    for (int i = 0; i < this.columns.length; i++) {
      newColumns[i] = this.columns[i].mask(rowsToKeep, cardinality);
    }

    return new Table(newColumns);
  }

  /**
   * Selects a subset of columns of this table, by names.
   *
   * @param colNames the column names to select
   * @return a table containing only selected columns
   */
  public Table selectColumns(List<String> colNames) {
    Column[] newCols =
        colNames.stream()
            .map(this::getColumnByName)
            .filter(Objects::nonNull)
            .toArray(Column[]::new);
    return new Table(newCols);
  }

  /**
   * Performs a join of this table with the right table, based on the provided conditions. The
   * parameters {@code keepLeftUnmatched}, {@code keepMatched} and {@code keepRightUnmatched}
   * control which rows should be returned. They can all be set to {@code false} to emulate an empty
   * result in erroneous conditions. The parameters {@code includeLeftColumns} and {@code
   * includeRightColumns} control which columns should be included in the result. In most cases they
   * will both be set to true. They allow to easily implement exclusive joins which only keep
   * columns form one table. {@code rightColumnsToDrop} allows to drop columns from the right table
   * that are redundant when joining on equality of equally named columns.
   */
  public Table join(
      Table right,
      List<JoinCondition> conditions,
      JoinKind joinKind,
      boolean includeLeftColumns,
      boolean includeRightColumns,
      List<String> rightColumnsToDrop,
      String right_prefix,
      ProblemAggregator problemAggregator) {
    NameDeduplicator nameDeduplicator = NameDeduplicator.createDefault(problemAggregator);

    JoinStrategy strategy = JoinStrategy.createStrategy(conditions, joinKind);
    JoinResult joinResult = strategy.join(problemAggregator);

    List<Column> newColumns = new ArrayList<>();

    if (includeLeftColumns) {
      OrderMask leftMask = joinResult.getLeftOrderMask();
      for (Column column : this.columns) {
        Column newColumn = column.applyMask(leftMask);
        newColumns.add(newColumn);
      }
    }

    if (includeRightColumns) {
      OrderMask rightMask = joinResult.getRightOrderMask();
      List<String> leftColumnNames = newColumns.stream().map(Column::getName).toList();

      HashSet<String> toDrop = new HashSet<>(rightColumnsToDrop);
      List<Column> rightColumnsToKeep =
          Arrays.stream(right.getColumns()).filter(col -> !toDrop.contains(col.getName())).toList();
      List<String> rightColumNames = rightColumnsToKeep.stream().map(Column::getName).toList();

      List<String> newRightColumnNames =
          nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, right_prefix);

      for (int i = 0; i < rightColumnsToKeep.size(); ++i) {
        Column column = rightColumnsToKeep.get(i);
        String newName = newRightColumnNames.get(i);
        Column newColumn = column.applyMask(rightMask).rename(newName);
        newColumns.add(newColumn);
      }
    }

    return new Table(newColumns.toArray(new Column[0]));
  }

  /** Performs a cross-join of this table with the right table. */
  public Table crossJoin(Table right, String rightPrefix, ProblemAggregator problemAggregator) {
    NameDeduplicator nameDeduplicator = NameDeduplicator.createDefault(problemAggregator);

    List<String> leftColumnNames =
        Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toList());
    List<String> rightColumNames =
        Arrays.stream(right.columns).map(Column::getName).collect(Collectors.toList());

    List<String> newRightColumnNames =
        nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, rightPrefix);

    JoinResult joinResult = CrossJoin.perform(this.rowCount(), right.rowCount());
    OrderMask leftMask = joinResult.getLeftOrderMask();
    OrderMask rightMask = joinResult.getRightOrderMask();

    Column[] newColumns = new Column[this.columns.length + right.columns.length];

    int leftColumnCount = this.columns.length;
    int rightColumnCount = right.columns.length;
    for (int i = 0; i < leftColumnCount; i++) {
      newColumns[i] = this.columns[i].applyMask(leftMask);
    }
    for (int i = 0; i < rightColumnCount; i++) {
      newColumns[leftColumnCount + i] =
          right.columns[i].applyMask(rightMask).rename(newRightColumnNames.get(i));
    }

    return new Table(newColumns);
  }

  /** Zips rows of this table with rows of the right table. */
  public Table zip(
      Table right, boolean keepUnmatched, String rightPrefix, ProblemAggregator problemAggregator) {
    NameDeduplicator nameDeduplicator = NameDeduplicator.createDefault(problemAggregator);

    int leftRowCount = this.rowCount();
    int rightRowCount = right.rowCount();
    int resultRowCount =
        keepUnmatched
            ? Math.max(leftRowCount, rightRowCount)
            : Math.min(leftRowCount, rightRowCount);

    List<String> leftColumnNames =
        Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toList());
    List<String> rightColumNames =
        Arrays.stream(right.columns).map(Column::getName).collect(Collectors.toList());
    List<String> newRightColumnNames =
        nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, rightPrefix);

    Column[] newColumns = new Column[this.columns.length + right.columns.length];

    int leftColumnCount = this.columns.length;
    int rightColumnCount = right.columns.length;
    for (int i = 0; i < leftColumnCount; i++) {
      newColumns[i] = this.columns[i].resize(resultRowCount);
    }
    for (int i = 0; i < rightColumnCount; i++) {
      newColumns[leftColumnCount + i] =
          right.columns[i].resize(resultRowCount).rename(newRightColumnNames.get(i));
    }

    return new Table(newColumns);
  }

  /**
   * Applies an order mask to all columns and indexes of this array.
   *
   * @param orderMask the mask to apply
   * @return a new table, with all columns and indexes reordered accordingly
   */
  public Table applyMask(OrderMask orderMask) {
    Column[] newColumns =
        Arrays.stream(columns)
            .map(
                column -> {
                  Storage<?> newStorage = column.getStorage().applyMask(orderMask);
                  return new Column(column.getName(), newStorage);
                })
            .toArray(Column[]::new);
    return new Table(newColumns);
  }

  /**
   * Transpose tables.
   *
   * @param id_columns the columns to use as the id values in the output.
   * @param to_transpose the columns to transpose.
   * @param name_field the name of the Name field in the output.
   * @param value_field the name of the Value field in the output.
   * @return a table result from transposing the specified columns.
   */
  public static Table transpose(
      Column[] id_columns,
      Column[] to_transpose,
      String name_field,
      String value_field,
      ProblemAggregator problemAggregator) {
    if (to_transpose.length == 0) {
      // Nothing to transpose, add two null columns to the existing set.
      Column[] newColumns = new Column[id_columns.length + 2];
      System.arraycopy(id_columns, 0, newColumns, 0, id_columns.length);

      int size = id_columns.length == 0 ? 0 : id_columns[0].getSize();
      Builder builder = new StringBuilder(size, TextType.VARIABLE_LENGTH);
      builder.appendNulls(size);
      Storage<?> newStorage = builder.seal();
      newColumns[id_columns.length] = new Column(name_field, newStorage);
      newColumns[id_columns.length + 1] = new Column(value_field, newStorage);
      return new Table(newColumns);
    }

    // Calculate Dimensions
    int size = to_transpose[0].getSize();
    int new_count = size * to_transpose.length;

    // Create Storage
    Builder[] storage = new Builder[id_columns.length + 2];
    IntStream.range(0, id_columns.length)
        .forEach(
            i ->
                storage[i] =
                    Builder.getForType(
                        id_columns[i].getStorage().getType(), new_count, problemAggregator));
    storage[id_columns.length] = new StringBuilder(new_count, TextType.VARIABLE_LENGTH);
    storage[id_columns.length + 1] = new InferredBuilder(new_count, problemAggregator);

    // Load Data
    Context context = Context.getCurrent();
    for (int row = 0; row < size; row++) {
      for (Column column : to_transpose) {
        for (int i = 0; i < id_columns.length; i++) {
          storage[i].append(id_columns[i].getStorage().getItemBoxed(row));
        }

        storage[id_columns.length].append(column.getName());
        storage[id_columns.length + 1].append(column.getStorage().getItemBoxed(row));
      }

      context.safepoint();
    }

    // Create Table
    Column[] new_columns = new Column[id_columns.length + 2];
    IntStream.range(0, id_columns.length)
        .forEach(i -> new_columns[i] = new Column(id_columns[i].getName(), storage[i].seal()));
    new_columns[id_columns.length] = new Column(name_field, storage[id_columns.length].seal());
    new_columns[id_columns.length + 1] =
        new Column(value_field, storage[id_columns.length + 1].seal());
    return new Table(new_columns);
  }

  /**
   * @return a copy of the Table containing a slice of the original data
   */
  public Table slice(int offset, int limit) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(offset, limit);
    }
    return new Table(newColumns);
  }

  /**
   * @return a copy of the Table consisting of slices of the original data
   */
  public Table slice(List<SliceRange> ranges) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(ranges);
    }
    return new Table(newColumns);
  }
}
