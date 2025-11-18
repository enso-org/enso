package org.enso.table.data.table;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.Text_Utils;
import org.enso.base.arrays.LongArrayList;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.masks.IndexMapper;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.index.CrossTabIndex;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.data.table.join.CrossJoin;
import org.enso.table.data.table.join.JoinKind;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.conditions.JoinCondition;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.operations.Distinct;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A representation of a table structure. */
public final class Table {
  private final Map<String, Column> columnNameMap = new HashMap<>();
  private final Column[] columns;
  private String versionId;

  /**
   * Creates a new table from a single column.
   *
   * @param column the column contained in this table.
   */
  public Table(Column column) {
    this(new Column[] {column});
  }

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

    assert checkAllColumnsHaveSameSize(columns) : "All columns must have the same row count.";

    this.columns = columns;
    this.versionId = UUID.randomUUID().toString();
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

  private static boolean checkAllColumnsHaveSameSize(Column[] columns) {
    int size = columns[0].getSize();
    for (Column column : columns) {
      if (column.getSize() != size) {
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
   * Get the row in this table.
   *
   * @param index value from 0 to {@link #rowCount()} (exclusive)
   * @return a row representing given index
   */
  public Row row(int index) {
    return new Row(this, index);
  }

  /**
   * @return the columns of this table
   */
  public Column[] getColumns() {
    return columns;
  }

  /**
   * @return the tables version id
   */
  public String getVersionId() {
    return versionId;
  }

  /**
   * Gets the value of a cell in the table by column name and row index. If the column does not
   * exist, it calls the provided function with the column name.
   *
   * @param columnName the name of the column
   * @param rowIndex the index of the row
   * @param ifMissing a function to call if the column is missing
   * @return the value of the cell, or the result of the function if the column is missing
   */
  public Object getValue(String columnName, long rowIndex, Function<String, Object> ifMissing) {
    Column column = getColumnByName(columnName);
    if (column == null) {
      return ifMissing.apply(columnName);
    }
    return column.getItem(rowIndex);
  }

  /**
   * Gets the value of a cell in the table by column index and row index. If the column does not
   * exist, it calls the provided function with the column name.
   *
   * @param columnIndex the index of the column
   * @param rowIndex the index of the row
   * @param ifMissing a function to call if the column is missing
   * @return the value of the cell, or the result of the function if the column is missing
   */
  public Object getValue(int columnIndex, long rowIndex, Function<Integer, Object> ifMissing) {
    if (columnIndex < 0 || columnIndex >= columns.length) {
      return ifMissing.apply(columnIndex);
    }
    return columns[columnIndex].getItem(rowIndex);
  }

  /**
   * Returns a column with the given name, or null if it doesn't exist.
   *
   * @param name the column name
   * @return a column with the given name
   */
  public Column getColumnByName(String name) {
    return columnNameMap.computeIfAbsent(
        name,
        columnName -> {
          for (Column column : columns) {
            if (Text_Utils.equals(column.getName(), columnName)) {
              return column;
            }
          }
          return null;
        });
  }

  /**
   * Returns a table resulting from selecting only the rows corresponding to true entries in the
   * provided column.
   *
   * @param filterColumn the column for selecting rows
   * @return the result of masking this table with the provided column
   */
  public Table filter(Column filterColumn) {
    if (filterColumn.getSize() > this.rowCount()) {
      // If given too many rows, we slice it to the size of the table.
      return filter(filterColumn.slice(0, this.rowCount()));
    }

    if (!(filterColumn.getStorage() instanceof ColumnBooleanStorage storage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }

    // Build a mask from the filter column.
    var maskBuilder = new LongArrayList((int) Math.min(storage.getSize(), 100000));
    StorageIterators.forEachOverBooleanStorage(
        storage,
        "filter",
        (index, value, isNothing) -> {
          if (value) {
            maskBuilder.add(index);
          }
          return false;
        });

    // The filter didn't remove any rows, so we return the table as is.
    if (maskBuilder.getSize() == this.rowCount()) {
      return this;
    }

    // Create a new table with the mask applied to all columns.
    var mask = maskBuilder.toArray();
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(mask);
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
   * Creates an index for this table by using values from the specified columns.
   *
   * @param columns set of columns to use as an index
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
   * @param where value identifying context where to execute the code
   * @param columns set of columns to use as an index
   * @param objectComparator Object comparator allowing calling back to `compare_to` when needed.
   * @return a table indexed by the proper column
   */
  public Table orderBy(
      Value where,
      ColumnStorage[] columns,
      Long[] directions,
      Comparator<Object> objectComparator) {
    var directionInts = Arrays.stream(directions).mapToInt(Long::intValue).toArray();
    var n = rowCount();
    var context = where.getContext();
    var storages = Arrays.stream(columns).toArray(ColumnStorage[]::new);
    var keys = new OrderedMultiValueKey[n];
    for (int i = 0; i < n; i++) {
      keys[i] = new OrderedMultiValueKey(storages, i, directionInts, objectComparator);
      context.safepoint();
    }
    Arrays.sort(keys);

    // Create a new mask
    boolean unchanged = true;
    long[] mask = new long[n];
    for (int i = 0; i < n; i++) {
      long newIndex = keys[i].getRowIndex();
      mask[i] = newIndex;
      if (newIndex != i) {
        unchanged = false;
      }
    }
    return unchanged ? this : this.mask(mask);
  }

  /**
   * Creates a new table keeping only rows with distinct key columns.
   *
   * @param keyColumns set of columns to use as an index
   * @param textFoldingStrategy a strategy for folding text columns
   * @param problemAggregator an aggregator for problems
   * @return a table where duplicate rows with the same key are removed
   */
  public Table distinct(
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      ProblemAggregator problemAggregator) {
    // If there are no key columns, we return the table as is.
    if (keyColumns.length == 0) {
      return this;
    }

    var rowsToKeep =
        Distinct.buildDistinctRowsMask(
            rowCount(), keyColumns, textFoldingStrategy, problemAggregator);
    Column[] newColumns = new Column[this.columns.length];
    for (int i = 0; i < this.columns.length; i++) {
      newColumns[i] = this.columns[i].mask(rowsToKeep);
    }
    return new Table(newColumns);
  }

  /**
   * Creates a new table keeping only rows with distinct key columns.
   *
   * @param keyColumns set of columns to use as an index
   * @param textFoldingStrategy a strategy for folding text columns
   * @param problemAggregator an aggregator for problems
   * @return a table where duplicate rows with the same key are removed
   */
  public Table duplicates(
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      ProblemAggregator problemAggregator) {
    // If there are no key columns, we return the table.
    if (keyColumns.length == 0) {
      return this;
    }

    var rowsToKeep =
        Distinct.buildDuplicatesRowsMask(
            rowCount(), keyColumns, textFoldingStrategy, problemAggregator);
    Column[] newColumns = new Column[this.columns.length];
    for (int i = 0; i < this.columns.length; i++) {
      newColumns[i] = this.columns[i].mask(rowsToKeep);
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

    var strategy = JoinStrategy.createStrategy(conditions, joinKind);
    var joinResult = strategy.join(problemAggregator);

    List<Column> newColumns = new ArrayList<>();

    if (includeLeftColumns) {
      var leftMask = joinResult.getLeftIndexMapper();
      for (Column column : this.columns) {
        var newColumn = column.mask(leftMask);
        newColumns.add(newColumn);
      }
    }

    if (includeRightColumns) {
      var rightMask = joinResult.getRightIndexMapper();
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
        var newColumn = column.mask(rightMask).rename(newName);
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

    var joinResult = CrossJoin.perform(this.rowCount(), right.rowCount());
    var leftMask = joinResult.getLeftIndexMapper();
    var rightMask = joinResult.getRightIndexMapper();

    Column[] newColumns = new Column[this.columns.length + right.columns.length];

    int leftColumnCount = this.columns.length;
    int rightColumnCount = right.columns.length;
    for (int i = 0; i < leftColumnCount; i++) {
      newColumns[i] = this.columns[i].mask(leftMask);
    }
    for (int i = 0; i < rightColumnCount; i++) {
      newColumns[leftColumnCount + i] =
          right.columns[i].mask(rightMask).rename(newRightColumnNames.get(i));
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
      newColumns[i] = resize(this.columns[i], resultRowCount);
    }
    for (int i = 0; i < rightColumnCount; i++) {
      newColumns[leftColumnCount + i] =
          resize(right.columns[i], resultRowCount).rename(newRightColumnNames.get(i));
    }

    return new Table(newColumns);
  }

  /**
   * Resizes the given column to the provided new length.
   *
   * <p>If the new length is smaller than the current length, the column is truncated. If the new
   * length is larger than the current length, the column is padded with nulls.
   */
  private static Column resize(Column input, int newSize) {
    var inputSize = input.getSize();
    if (inputSize == newSize) {
      return input;
    }

    if (newSize < inputSize) {
      return input.slice(0, newSize);
    }

    var storage = input.getStorage();
    var builder = storage.getType().makeBuilder(newSize, BlackholeProblemAggregator.INSTANCE);
    builder.appendBulkStorage(storage);
    builder.appendNulls(newSize - inputSize);
    return new Column(input.getName(), builder.seal());
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
      var builder = Builder.getForText(TextType.VARIABLE_LENGTH, size);
      builder.appendNulls(size);
      var newStorage = builder.seal();
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
    storage[id_columns.length] = Builder.getForText(TextType.VARIABLE_LENGTH, new_count);
    storage[id_columns.length + 1] = Builder.getInferredBuilder(new_count, problemAggregator);

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
  public Table slice(long offset, long limit) {
    var indexMapper =
        offset > rowCount()
            ? new IndexMapper.SingleSlice(0, 0)
            : new IndexMapper.SingleSlice(offset, limit);
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(indexMapper);
    }
    return new Table(newColumns);
  }

  /**
   * @return a copy of the Table consisting of slices of the original data
   */
  public Table slice(List<SliceRange> ranges) {
    if (ranges.isEmpty()) {
      // Creates an empty table
      return slice(0, 0);
    }

    if (ranges.size() == 1) {
      // If there is only one range, we can use the existing slice method
      SliceRange range = ranges.get(0);
      return slice(range.start(), range.end() - range.start());
    }

    // Now we have to form multiple parts so create a mask
    long[] mask = SliceRange.createMask(ranges);
    return mask(mask);
  }

  public Table mask(long[] mask) {
    var indexMapper = new IndexMapper.ArrayMapping(mask);
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(indexMapper);
    }
    return new Table(newColumns);
  }

  public Table reverse() {
    var indexMapper = new IndexMapper.Reversed(0, rowCount());
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(indexMapper);
    }
    return new Table(newColumns);
  }
}
