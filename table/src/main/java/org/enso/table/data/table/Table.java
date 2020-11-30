package org.enso.table.data.table;

import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.Index;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.HashIndex;
import org.enso.table.error.NoSuchColumnException;
import org.enso.table.error.UnexpectedColumnTypeException;

import java.util.*;
import java.util.stream.Collectors;

/** A representation of a table structure. */
public class Table {

  private final Column[] columns;
  private final Index index;

  /**
   * Creates a new table
   *
   * @param columns the columns contained in this table.
   */
  public Table(Column[] columns) {
    this(
        columns,
        new DefaultIndex(
            (columns == null || columns.length == 0) ? 0 : (int) columns[0].getSize()));
  }

  private Table(Column[] columns, Index index) {
    this.columns = columns;
    this.index = index;
  }

  /** @return the number of rows in this table */
  public long nrows() {
    if (columns == null || columns.length == 0) {
      return 0;
    } else {
      return columns[0].getStorage().size();
    }
  }

  /** @return the columns of this table */
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
      if (column.getName().equals(name)) {
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
    if (!(maskCol.getStorage() instanceof BoolStorage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }
    BoolStorage storage = (BoolStorage) maskCol.getStorage();
    BitSet mask = new BitSet();
    mask.or(storage.getValues());
    if (storage.isNegated()) {
      mask.flip(0, (int) storage.size());
    }
    mask.andNot(storage.getIsMissing());
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
      if (columns[i].getName().equals(newColumn.getName())) {
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
   * @param name the column name to use as index
   * @return a table indexed by the proper column
   */
  public Table indexFromColumn(String name) {
    Column col = getColumnByName(name);
    if (col == null) throw new NoSuchColumnException(name);
    Storage storage = col.getStorage();
    Index ix = HashIndex.fromStorage(col.getName(), storage);
    Column[] newColumns = new Column[columns.length - 1];
    int j = 0;
    for (int i = 0; i < columns.length; i++) {
      if (!columns[i].getName().equals(name)) {
        newColumns[j++] = columns[i].withIndex(ix);
      }
    }
    return new Table(newColumns, ix);
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
    return new Table(newCols, index);
  }

  /**
   * Joins this table with another, by combining rows from this with rows of other with a matching
   * index.
   *
   * @param other the table being joined with
   * @param dropUnmatched whether the rows containing unmatched values in this should be dropped
   * @param on a column name in this that should be used as the join key. If this is null, index is
   *     used instead
   * @param lsuffix the suffix to add to names of columns of this in case there's a name conflict
   * @param rsuffix the suffix to add to names of columns of other in case there's a name conflict
   * @return the result of performing the join
   */
  @SuppressWarnings("unchecked")
  public Table join(Table other, boolean dropUnmatched, String on, String lsuffix, String rsuffix) {
    int s = (int) nrows();
    List<Integer>[] matches = new List[s];
    if (on == null) {
      for (int i = 0; i < s; i++) {
        matches[i] = other.index.loc(index.iloc(i));
      }
    } else {
      Storage onS = getColumnByName(on).getStorage();
      for (int i = 0; i < s; i++) {
        matches[i] = other.index.loc(onS.getItemBoxed(i));
      }
    }
    int outSize = 0;
    int[] countMask = new int[s];
    for (int i = 0; i < s; i++) {
      if (matches[i] == null) {
        countMask[i] = dropUnmatched ? 0 : 1;
      } else {
        countMask[i] = matches[i].size();
      }
      outSize += countMask[i];
    }
    int[] orderMask = new int[outSize];
    int orderMaskPosition = 0;
    for (int i = 0; i < s; i++) {
      if (matches[i] == null) {
        if (!dropUnmatched) {
          orderMask[orderMaskPosition++] = Index.NOT_FOUND;
        }
      } else {
        for (Integer x : matches[i]) {
          orderMask[orderMaskPosition++] = x;
        }
      }
    }
    Column[] newColumns = new Column[this.columns.length + other.columns.length];
    Index newIndex = index.countMask(countMask, outSize);
    Set<String> lnames =
        Arrays.stream(this.columns).map(Column::getName).collect(Collectors.toSet());
    Set<String> rnames =
        Arrays.stream(other.columns).map(Column::getName).collect(Collectors.toSet());
    for (int i = 0; i < columns.length; i++) {
      Column original = columns[i];
      newColumns[i] =
          new Column(
              suffixIfNecessary(rnames, original.getName(), lsuffix),
              newIndex,
              original.getStorage().countMask(countMask, outSize));
    }
    for (int i = 0; i < other.columns.length; i++) {
      Column original = other.columns[i];
      newColumns[i + columns.length] =
          new Column(
              suffixIfNecessary(lnames, original.getName(), rsuffix),
              newIndex,
              original.getStorage().orderMask(orderMask));
    }
    return new Table(newColumns, newIndex);
  }

  private String suffixIfNecessary(Set<String> names, String name, String suffix) {
    return names.contains(name) ? name + suffix : name;
  }
}
