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
import org.enso.table.data.table.join.IndexJoin;
import org.enso.table.data.table.join.JoinCondition;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.operations.Distinct;
import org.enso.table.util.NameDeduplicator;

import java.util.*;
import java.util.function.BiFunction;
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
    this.columns = columns;
    this.problems = problems;
  }

  /** @return the number of rows in this table */
  public int rowCount() {
    // TODO I think we can make this check obsolete once we start requiring >=1 column in tables.
    if (columns.length == 0) {
      return 0;
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
   *
   * The parameters {@code keepLeftUnmatched}, {@code keepMatched} and {@code keepRightUnmatched} control which rows should be returned. They can all be set to {@code false} to emulate an empty result in erroneous conditions.
   *
   * The parameters {@code includeLeftColumns} and {@code includeRightColumns} control which columns should be included in the result. In most cases they will both be set to true. They allow to easily implement exclusive joins which only keep columns form one table.
   *
   * {@code rightColumnsToDrop} allows to drop columns from the right table that are redundant when joining on equality of equally named columns.
   */
  public Table join(Table right, List<JoinCondition> conditions, boolean keepLeftUnmatched, boolean keepMatched, boolean keepRightUnmatched, boolean includeLeftColumns, boolean includeRightColumns, List<String> rightColumnsToDrop, String right_prefix, Comparator<Object> objectComparator, BiFunction<Object, Object, Boolean> equalityFallback) {
    NameDeduplicator nameDeduplicator = new NameDeduplicator();
    JoinResult joinResult = null;
    // Only compute the join if there are any results to be returned.
    if (keepLeftUnmatched || keepMatched || keepRightUnmatched) {
      JoinStrategy strategy = new IndexJoin(objectComparator, equalityFallback);
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
        Column newColumn = column.applyMask(leftMask);
        newColumns.add(newColumn);
      }
    }

    if (includeRightColumns) {
      List<String> leftColumnNames = newColumns.stream().map(Column::getName).collect(Collectors.toList());

      HashSet<String> toDrop = new HashSet<>(rightColumnsToDrop);
      List<Column> rightColumnsToKeep = Arrays.stream(right.getColumns()).filter(col -> !toDrop.contains(col.getName())).collect(Collectors.toList());
      List<String> rightColumNames = rightColumnsToKeep.stream().map(Column::getName).collect(Collectors.toList());

      List<String> newRightColumnNames = nameDeduplicator.combineWithPrefix(leftColumnNames, rightColumNames, right_prefix);

      for (int i = 0; i < rightColumnsToKeep.size(); ++i) {
        Column column = rightColumnsToKeep.get(i);
        String newName = newRightColumnNames.get(i);
        Storage<?> newStorage = column.getStorage().applyMask(rightMask);
        Column newColumn = new Column(newName, newStorage);
        newColumns.add(newColumn);
      }
    }

    AggregatedProblems joinProblems = joinResult != null ? joinResult.problems() : null;
    AggregatedProblems aggregatedProblems = AggregatedProblems.merge(joinProblems, AggregatedProblems.of(nameDeduplicator.getProblems()));
    return new Table(newColumns.toArray(new Column[0]), aggregatedProblems);
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
              suffixIfNecessary(rnames, original.getName(), lsuffix), original.getStorage());
    }
    for (int i = 0; i < other.columns.length; i++) {
      Column original = other.columns[i];
      newColumns[i + columns.length] =
          new Column(
              suffixIfNecessary(lnames, original.getName(), rsuffix), original.getStorage());
    }
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
