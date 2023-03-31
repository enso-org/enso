package org.enso.table.data.table;

import org.enso.base.Text_Utils;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.Index;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.data.table.join.CrossJoin;
import org.enso.table.data.table.join.IndexJoin;
import org.enso.table.data.table.join.JoinCondition;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.operations.Distinct;
import org.enso.table.util.NameDeduplicator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** A representation of a table structure. */
public class Table {

  private final Column[] columns;
  private final AggregatedProblems problems;

  /**
   * Creates a new table
   *
   * @param columns the columns contained in this table.
   */
  public Table(Column[] columns) {
    this(columns, null);
  }

  public Table(Column[] columns, AggregatedProblems problems) {
    if (columns.length == 0) {
      throw new IllegalArgumentException("A Table must have at least one column.");
    }

    if (!checkUniqueColumns(columns)) {
      throw new IllegalArgumentException("Column names must be unique within a Table.");
    }

    this.columns = columns;
    this.problems = problems;
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

  /** @return the number of rows in this table */
  public int rowCount() {
    return columns[0].getSize();
  }

  /** @return the columns of this table */
  public Column[] getColumns() {
    return columns;
  }

  /** @return Attached set of any problems from the Java side */
  public AggregatedProblems getProblems() {
    return problems;
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
    return new Table(newColumns, null);
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
    return new Table(newCols, null);
  }

  private Table addColumn(Column newColumn) {
    Column[] newCols = new Column[columns.length + 1];
    System.arraycopy(columns, 0, newCols, 0, columns.length);
    newCols[columns.length] = newColumn;
    return new Table(newCols, null);
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
  public MultiValueIndex<?> indexFromColumns(Column[] columns) {
    return MultiValueIndex.makeUnorderedIndex(columns, this.rowCount(), TextFoldingStrategy.unicodeNormalizedFold);
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
    MultiValueIndex<?> index = MultiValueIndex.makeOrderedIndex(columns, this.rowCount(), directionInts, objectComparator);
    OrderMask mask = new OrderMask(index.makeOrderMap(this.rowCount()));
    return this.applyMask(mask);
  }

  /**
   * Creates a new table keeping only rows with distinct key columns.
   *
   * @param keyColumns set of columns to use as an Index
   * @param textFoldingStrategy a strategy for folding text columns
   * @return a table where duplicate rows with the same key are removed
   */
  public Table distinct(Column[] keyColumns, TextFoldingStrategy textFoldingStrategy) {
    var problems = new AggregatedProblems();
    var rowsToKeep = Distinct.buildDistinctRowsMask(rowCount(), keyColumns, textFoldingStrategy, problems);
    int cardinality = rowsToKeep.cardinality();
    Column[] newColumns = new Column[this.columns.length];
    for (int i = 0; i < this.columns.length; i++) {
      newColumns[i] = this.columns[i].mask(rowsToKeep, cardinality);
    }

    return new Table(newColumns, problems);
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
    return new Table(newCols, null);
  }

  /**
   * Performs a join of this table with the right table, based on the provided
   * conditions.
   * The parameters {@code keepLeftUnmatched}, {@code keepMatched} and {@code keepRightUnmatched} control which rows should be returned. They can all be set to {@code false} to emulate an empty result in erroneous conditions.
   * The parameters {@code includeLeftColumns} and {@code includeRightColumns} control which columns should be included in the result. In most cases they will both be set to true. They allow to easily implement exclusive joins which only keep columns form one table.
   * {@code rightColumnsToDrop} allows to drop columns from the right table that are redundant when joining on equality of equally named columns.
   */
  public Table join(Table right, List<JoinCondition> conditions, boolean keepLeftUnmatched, boolean keepMatched, boolean keepRightUnmatched, boolean includeLeftColumns, boolean includeRightColumns, List<String> rightColumnsToDrop, String right_prefix) {
    NameDeduplicator nameDeduplicator = new NameDeduplicator();
    if (!keepLeftUnmatched && !keepMatched && !keepRightUnmatched) {
      throw new IllegalArgumentException("At least one of keepLeftUnmatched, keepMatched or keepRightUnmatched must be true.");
    }

    var strategy = new IndexJoin();
    JoinResult joinResult = strategy.join(this, right, conditions);

    List<JoinResult> resultsToKeep = new ArrayList<>();

    if (keepMatched) {
      resultsToKeep.add(joinResult);
    }

    if (keepLeftUnmatched) {
      Set<Integer> matchedLeftRows = joinResult.leftMatchedRows();
      JoinResult.Builder leftUnmatchedBuilder = new JoinResult.Builder();
      for (int i = 0; i < this.rowCount(); i++) {
        if (!matchedLeftRows.contains(i)) {
          leftUnmatchedBuilder.addRow(i, Index.NOT_FOUND);
        }
      }

      resultsToKeep.add(leftUnmatchedBuilder.build(AggregatedProblems.of()));
    }

    if (keepRightUnmatched) {
      Set<Integer> matchedRightRows = joinResult.rightMatchedRows();
      JoinResult.Builder rightUnmatchedBuilder = new JoinResult.Builder();
      for (int i = 0; i < right.rowCount(); i++) {
        if (!matchedRightRows.contains(i)) {
          rightUnmatchedBuilder.addRow(Index.NOT_FOUND, i);
        }
      }

      resultsToKeep.add(rightUnmatchedBuilder.build(AggregatedProblems.of()));
    }

    List<Column> newColumns = new ArrayList<>();

    if (includeLeftColumns) {
      OrderMask leftMask = OrderMask.concat(resultsToKeep.stream().map(JoinResult::getLeftOrderMask).collect(Collectors.toList()));
      for (Column column : this.columns) {
        Column newColumn = column.applyMask(leftMask);
        newColumns.add(newColumn);
      }
    }

    if (includeRightColumns) {
      OrderMask rightMask = OrderMask.concat(resultsToKeep.stream().map(JoinResult::getRightOrderMask).collect(Collectors.toList()));
      List<String> leftColumnNames = newColumns.stream().map(Column::getName).collect(Collectors.toList());

      HashSet<String> toDrop = new HashSet<>(rightColumnsToDrop);
      List<Column> rightColumnsToKeep = Arrays.stream(right.getColumns()).filter(col -> !toDrop.contains(col.getName())).collect(Collectors.toList());
      List<String> rightColumNames = rightColumnsToKeep.stream().map(Column::getName).collect(Collectors.toList());

      List<String> newRightColumnNames = nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, right_prefix);

      for (int i = 0; i < rightColumnsToKeep.size(); ++i) {
        Column column = rightColumnsToKeep.get(i);
        String newName = newRightColumnNames.get(i);
        Column newColumn = column.applyMask(rightMask).rename(newName);
        newColumns.add(newColumn);
      }
    }

    AggregatedProblems joinProblems = joinResult != null ? joinResult.problems() : null;
    AggregatedProblems aggregatedProblems = AggregatedProblems.merge(AggregatedProblems.of(nameDeduplicator.getProblems()), joinProblems);
    return new Table(newColumns.toArray(new Column[0]), aggregatedProblems);
  }

  /**
   * Performs a cross-join of this table with the right table.
   */
  public Table crossJoin(Table right, String rightPrefix) {
    NameDeduplicator nameDeduplicator = new NameDeduplicator();

    List<String> leftColumnNames = Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toList());
    List<String> rightColumNames = Arrays.stream(right.columns).map(Column::getName).collect(Collectors.toList());

    List<String> newRightColumnNames = nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, rightPrefix);

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
      newColumns[leftColumnCount + i] = right.columns[i].applyMask(rightMask).rename(newRightColumnNames.get(i));
    }

    AggregatedProblems aggregatedProblems = AggregatedProblems.merge(AggregatedProblems.of(nameDeduplicator.getProblems()), joinResult.problems());
    return new Table(newColumns, aggregatedProblems);
  }

  /**
   * Zips rows of this table with rows of the right table.
   */
  public Table zip(Table right, boolean keepUnmatched, String rightPrefix) {
    NameDeduplicator nameDeduplicator = new NameDeduplicator();

    int leftRowCount = this.rowCount();
    int rightRowCount = right.rowCount();
    int resultRowCount = keepUnmatched ? Math.max(leftRowCount, rightRowCount) : Math.min(leftRowCount, rightRowCount);

    List<String> leftColumnNames = Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toList());
    List<String> rightColumNames = Arrays.stream(right.columns).map(Column::getName).collect(Collectors.toList());
    List<String> newRightColumnNames = nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, rightPrefix);

    Column[] newColumns = new Column[this.columns.length + right.columns.length];

    int leftColumnCount = this.columns.length;
    int rightColumnCount = right.columns.length;
    for (int i = 0; i < leftColumnCount; i++) {
      newColumns[i] = this.columns[i].resize(resultRowCount);
    }
    for (int i = 0; i < rightColumnCount; i++) {
      newColumns[leftColumnCount + i] = right.columns[i].resize(resultRowCount).rename(newRightColumnNames.get(i));
    }

    return new Table(newColumns, AggregatedProblems.of(nameDeduplicator.getProblems()));
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
    return new Table(newColumns, null);
  }

  private static class NamedBuilder {
    private final String name;
    private final InferredBuilder builder;

    private NamedBuilder(String name, int size) {
      this.name = name;
      this.builder = new InferredBuilder(size);
    }
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
  public static Table transpose(Column[] id_columns, Column[] to_transpose, String name_field, String value_field) {
    if (to_transpose.length == 0) {
      // Nothing to transpose, add two null columns to the existing set.
      Column[] newColumns = new Column[id_columns.length + 2];
      System.arraycopy(id_columns, 0, newColumns, 0, id_columns.length);

      int size = id_columns.length == 0 ? 0 : id_columns[0].getSize();
      Builder builder = new StringBuilder(size);
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
    IntStream.range(0, id_columns.length).forEach(i -> storage[i] = Builder.getForType(id_columns[i].getStorage().getType(), new_count));
    storage[id_columns.length] = new StringBuilder(new_count);
    storage[id_columns.length + 1] = new InferredBuilder(new_count);

    // Load Data
    for (int row = 0; row < size; row++) {
      for (Column column : to_transpose) {
        for (int i = 0; i < id_columns.length; i++) {
          storage[i].append(id_columns[i].getStorage().getItemBoxed(row));
        }

        storage[id_columns.length].append(column.getName());
        storage[id_columns.length + 1].append(column.getStorage().getItemBoxed(row));
      }
    }

    // Create Table
    Column[] new_columns = new Column[id_columns.length + 2];
    IntStream.range(0, id_columns.length).forEach(i -> new_columns[i] = new Column(id_columns[i].getName(), storage[i].seal()));
    new_columns[id_columns.length] = new Column(name_field, storage[id_columns.length].seal());
    new_columns[id_columns.length + 1] = new Column(value_field, storage[id_columns.length + 1].seal());
    return new Table(new_columns);
  }

  /**
   * Concatenates tables. Any column that is present in one table, but missing in another, will be
   * {@code null}-padded in the positions corresponding to the missing column.
   *
   * @param tables the (non-empty) list of tables to concatenate.
   * @return a table result from concatenating both tables
   */
  public static Table concat(List<Table> tables) {
    int resultSize = tables.stream().mapToInt(Table::rowCount).sum();

    List<NamedBuilder> builders = new ArrayList<>();
    int completedRows = 0;
    for (var table : tables) {
      for (var column : table.getColumns()) {
        var matchingBuilder =
            builders.stream().filter(bldr -> Text_Utils.equals(bldr.name, column.getName())).findFirst();
        NamedBuilder builder;
        if (matchingBuilder.isPresent()) {
          builder = matchingBuilder.get();
        } else {
          builder = new NamedBuilder(column.getName(), resultSize);
          builders.add(builder);
          builder.builder.appendNulls(completedRows);
        }
        var storage = column.getStorage();
        for (int i = 0; i < storage.size(); i++) {
          builder.builder.appendNoGrow(storage.getItemBoxed(i));
        }
      }
      for (var builder : builders) {
        var columnExists =
            Arrays.stream(table.getColumns()).anyMatch(col -> Text_Utils.equals(col.getName(), builder.name));
        if (!columnExists) {
          builder.builder.appendNulls(table.rowCount());
        }
      }
      completedRows += table.rowCount();
    }

    Column[] newColumns =
        builders.stream()
            .map(builder -> new Column(builder.name, builder.builder.seal()))
            .toArray(Column[]::new);
    return new Table(newColumns, null);
  }
  /** @return a copy of the Table containing a slice of the original data */
  public Table slice(int offset, int limit) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(offset, limit);
    }
    return new Table(newColumns, null);
  }

  /** @return a copy of the Table consisting of slices of the original data */
  public Table slice(List<SliceRange> ranges) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(ranges);
    }
    return new Table(newColumns, null);
  }
}
