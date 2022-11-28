package org.enso.table.data.table;

import org.enso.base.Text_Utils;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.HashIndex;
import org.enso.table.data.index.Index;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.data.table.join.JoinCondition;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.ScanJoin;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.error.NoSuchColumnException;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.operations.Distinct;
import org.enso.table.util.NameDeduplicator;

import java.util.*;
import java.util.stream.Collectors;

/** A representation of a table structure. */
public class Table {

  private final Column[] columns;
  @Deprecated
  private final Index index;
  private final AggregatedProblems problems;

  /**
   * Creates a new table
   *
   * @param columns the columns contained in this table.
   */
  public Table(Column[] columns) {
    this(columns, null, null);
  }

  public Table(Column[] columns, Index index) {
    this(columns, index, null);
  }

  public Table(Column[] columns, AggregatedProblems problems) {
    this(columns, null, problems);
  }

  private Table(Column[] columns, Index index, AggregatedProblems problems) {
    this.columns = columns;
    this.index =
        index == null
            ? (new DefaultIndex(
                (columns == null || columns.length == 0) ? 0 : columns[0].getSize()))
            : index;
    this.problems = problems;
  }

  /** @return the number of rows in this table */
  public int rowCount() {
    if (columns == null || columns.length == 0) {
      return index.size();
    } else {
      return columns[0].getSize();
    }
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
   * Returns a column or index with the given name, or null if it doesn't exist.
   *
   * @param name the column name
   * @return a column or index column with the given name
   */
  public Column getColumnOrIndexByName(String name) {
    var column = getColumnByName(name);
    if (column != null) {
      return column;
    }

    if (Text_Utils.equals(getIndex().getName(), name)) {
      return getIndex().toColumn();
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
    Index newIx = index.mask(mask, cardinality);
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(newIx, mask, cardinality);
    }
    return new Table(newColumns, newIx);
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
    return new Table(newCols, index);
  }

  private Table addColumn(Column newColumn) {
    Column[] newCols = new Column[columns.length + 1];
    System.arraycopy(columns, 0, newCols, 0, columns.length);
    newCols[columns.length] = newColumn;
    return new Table(newCols, index);
  }

  /**
   * Returns the index of this table.
   *
   * @return the index of this table
   */
  public Index getIndex() {
    return index;
  }

  /**
   * Reindexes this table by using values from the column with the given name.
   *
   * @param col the column to use as index
   * @return a table indexed by the proper column
   */
  public Table indexFromColumn(Column col) {
    Storage<?> storage = col.getStorage();
    Index ix = HashIndex.fromStorage(col.getName(), storage);
    List<Column> newColumns = new ArrayList<>();
    Column indexCol = index.toColumn();
    if (indexCol != null) {
      newColumns.add(indexCol.withIndex(ix));
    }
    for (Column column : columns) {
      if (!Text_Utils.equals(column.getName(), col.getName())) {
        newColumns.add(column.withIndex(ix));
      }
    }
    return new Table(newColumns.toArray(new Column[0]), ix);
  }

  /**
   * Reindexes this table by using values from the column with the given name.
   *
   * @param name the column name to use as index
   * @return a table indexed by the proper column
   */
  public Table indexFromColumn(String name) {
    Column col = getColumnByName(name);
    if (col == null) throw new NoSuchColumnException(name);
    return indexFromColumn(col);
  }

  /**
   * Creates an index for this table by using values from the specified columns.
   *
   * @param columns set of columns to use as an Index
   * @param objectComparator Object comparator allowing calling back to `compare_to` when needed.
   * @return a table indexed by the proper column
   */
  public MultiValueIndex indexFromColumns(Column[] columns, Comparator<Object> objectComparator) {
    return new MultiValueIndex(columns, this.rowCount(), objectComparator);
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
    MultiValueIndex index = new MultiValueIndex(columns, this.rowCount(), directionInts, objectComparator);
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
    Index newIx = index.mask(rowsToKeep, cardinality);
    for (int i = 0; i < this.columns.length; i++) {
      newColumns[i] = this.columns[i].mask(newIx, rowsToKeep, cardinality);
    }

    return new Table(newColumns, newIx, problems);
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
            .map(this::getColumnOrIndexByName)
            .filter(Objects::nonNull)
            .toArray(Column[]::new);
    return new Table(newCols, index);
  }

  /**
   * Performs a join of this table with the right table, based on the provided
   * conditions.
   *
   * The parameters {@code keepLeftUnmatched}, {@code keepMatched} and {@code keepRightUnmatched} control which rows should be returned. They can all be set to {@code false} to emulate an empty result in erroneous conditions.
   *
   * The parameters {@code includeLeftColumns} and {@code includeRightColumns} control which columns should be included in the result. In most cases they will both be set to true. They allow to easily implement exclusive joins which only keep columns form one table.
   *
   * {@code rightColumnsToDrop} allows to drop columns from the right table that are redundant when joining on equality of equally named columns.
   */
  public Table join(Table right, List<JoinCondition> conditions, boolean keepLeftUnmatched, boolean keepMatched, boolean keepRightUnmatched, boolean includeLeftColumns, boolean includeRightColumns, List<String> rightColumnsToDrop, String right_prefix) {
    // TODO adding prefix for right columns
    NameDeduplicator deduplicator = new NameDeduplicator();

    JoinStrategy strategy = new ScanJoin();
    JoinResult joinResult = null;
    // Only compute the join if there are any results to be returned.
    if (keepLeftUnmatched || keepMatched || keepRightUnmatched) {
      joinResult = strategy.join(this, right, conditions);
    }

    List<Integer> leftRows = new ArrayList<>();
    List<Integer> rightRows = new ArrayList<>();

    if (keepMatched) {
      for (var match : joinResult.matchedRows()) {
        leftRows.add(match.getLeft());
        rightRows.add(match.getRight());
      }
    }

    if (keepLeftUnmatched) {
      HashSet<Integer> matchedLeftRows = new HashSet<>();
      for (var match : joinResult.matchedRows()) {
        matchedLeftRows.add(match.getLeft());
      }

      for (int i = 0; i < this.rowCount(); i++) {
        if (!matchedLeftRows.contains(i)) {
          leftRows.add(i);
          rightRows.add(Index.NOT_FOUND);
        }
      }
    }

    if (keepRightUnmatched) {
      HashSet<Integer> matchedRightRows = new HashSet<>();
      for (var match : joinResult.matchedRows()) {
        matchedRightRows.add(match.getRight());
      }

      for (int i = 0; i < right.rowCount(); i++) {
        if (!matchedRightRows.contains(i)) {
          leftRows.add(Index.NOT_FOUND);
          rightRows.add(i);
        }
      }
    }

    OrderMask leftMask = OrderMask.fromList(leftRows);
    OrderMask rightMask = OrderMask.fromList(rightRows);

    List<Column> newColumns = new ArrayList<>();

    if (includeLeftColumns) {
      for (Column column : this.columns) {
        deduplicator.markUsed(column.getName());
        Column newColumn = column.applyMask(leftMask);
        newColumns.add(newColumn);
      }
    }

    if (includeRightColumns) {
      HashSet<String> toDrop = new HashSet<>(rightColumnsToDrop);
      for (Column column : right.getColumns()) {
        if (toDrop.contains(column.getName())) {
          continue;
        }

        // TODO drop columns from Equals conditions if equal names too
        String newName = deduplicator.makeUnique(column.getName());
        Storage<?> newStorage = column.getStorage().applyMask(rightMask);
        Column newColumn = new Column(newName, newStorage);
        newColumns.add(newColumn);
      }
    }

    return new Table(newColumns.toArray(new Column[0]));
  }

  /**
   * Applies an order mask to all columns and indexes of this array.
   *
   * @param orderMask the mask to apply
   * @return a new table, with all columns and indexes reordered accordingly
   */
  public Table applyMask(OrderMask orderMask) {
    final Index newIndex = index.applyMask(orderMask);
    Column[] newColumns =
        Arrays.stream(columns)
            .map(
                column -> {
                  Storage<?> newStorage = column.getStorage().applyMask(orderMask);
                  return new Column(column.getName(), newIndex, newStorage);
                })
            .toArray(Column[]::new);
    return new Table(newColumns, newIndex);
  }

  private String suffixIfNecessary(Set<String> names, String name, String suffix) {
    return names.contains(name) ? name + suffix : name;
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
   * Concatenates tables. Any column that is present in one table, but missing in another, will be
   * {@code null}-padded in the positions corresponding to the missing column.
   *
   * @param tables the (non-empty) list of tables to concatenate.
   * @return a table result from concatenating both tables
   */
  public static Table concat(List<Table> tables) {

    Index newIndex =
        concatIndexes(tables.stream().map(Table::getIndex).collect(Collectors.toList()));

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
            .map(builder -> new Column(builder.name, newIndex, builder.builder.seal()))
            .toArray(Column[]::new);
    return new Table(newColumns, newIndex);
  }

  private Storage<?> concatStorages(Storage<?> left, Storage<?> right) {
    InferredBuilder builder = new InferredBuilder(left.size() + right.size());
    for (int i = 0; i < left.size(); i++) {
      builder.appendNoGrow(left.getItemBoxed(i));
    }
    for (int j = 0; j < right.size(); j++) {
      builder.appendNoGrow(right.getItemBoxed(j));
    }
    return builder.seal();
  }

  private Storage<?> nullPad(int nullCount, Storage<?> storage, boolean start) {
    InferredBuilder builder = new InferredBuilder(nullCount + storage.size());
    if (start) {
      builder.appendNulls(nullCount);
    }
    for (int i = 0; i < storage.size(); i++) {
      builder.appendNoGrow(storage.getItemBoxed(i));
    }
    if (!start) {
      builder.appendNulls(nullCount);
    }
    return builder.seal();
  }

  private static Index concatIndexes(List<Index> indexes) {
    int resultSize = indexes.stream().mapToInt(Index::size).sum();
    if (indexes.stream().allMatch(ix -> ix instanceof DefaultIndex)) {
      return new DefaultIndex(resultSize);
    } else {
      InferredBuilder builder = new InferredBuilder(resultSize);
      for (var index : indexes) {
        for (int i = 0; i < index.size(); i++) {
          builder.appendNoGrow(index.iloc(i));
        }
      }
      return HashIndex.fromStorage(indexes.get(0).getName(), builder.seal());
    }
  }

  private Table hconcat(Table other, String lsuffix, String rsuffix) {
    Column[] newColumns = new Column[this.columns.length + other.columns.length];
    Set<String> lnames =
        Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toSet());
    Set<String> rnames =
        Arrays.stream(other.columns).map(Column::getName).collect(Collectors.toSet());
    for (int i = 0; i < columns.length; i++) {
      Column original = columns[i];
      newColumns[i] =
          new Column(
              suffixIfNecessary(rnames, original.getName(), lsuffix), index, original.getStorage());
    }
    for (int i = 0; i < other.columns.length; i++) {
      Column original = other.columns[i];
      newColumns[i + columns.length] =
          new Column(
              suffixIfNecessary(lnames, original.getName(), rsuffix), index, original.getStorage());
    }
    return new Table(newColumns, index);
  }

  /** @return a copy of the Table containing a slice of the original data */
  public Table slice(int offset, int limit) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(offset, limit);
    }
    return new Table(newColumns, index.slice(offset, limit));
  }

  /** @return a copy of the Table consisting of slices of the original data */
  public Table slice(List<SliceRange> ranges) {
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].slice(ranges);
    }
    return new Table(newColumns, index.slice(ranges));
  }
}
