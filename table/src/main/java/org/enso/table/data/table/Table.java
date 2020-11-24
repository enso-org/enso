package org.enso.table.data.table;

import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.index.Index;
import org.enso.table.data.index.LongIndex;
import org.enso.table.data.index.NoIndex;
import org.enso.table.data.index.StringIndex;
import org.enso.table.error.UnexpectedColumnTypeException;

import java.util.BitSet;
import java.util.List;

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
    this(columns, new NoIndex());
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

  public Index getIndex() {
    return index;
  }

  public Table indexFromColumn(String name) {
    Column col = getColumnByName(name);
    if (col == null) throw new RuntimeException("No column called " + name);
    if (col.getStorage() instanceof StringStorage) {
      StringStorage storage = (StringStorage) col.getStorage();
      Index ix = StringIndex.fromStorage(col.getName(), storage);
      Column[] newColumns = new Column[columns.length - 1];
      int j = 0;
      for (int i = 0; i < columns.length; i++) {
        if (!columns[i].getName().equals(name)) {
          newColumns[j++] = columns[i].withIndex(ix);
        }
      }
      return new Table(newColumns, ix);
    } else if (col.getStorage() instanceof LongStorage) {
      LongIndex.fromStorage((LongStorage) col.getStorage());
      throw new RuntimeException("Unsupported column type for index");
    } else {
      throw new RuntimeException("Unsupported column type for index");
    }
  }

  public Table selectColumns(List<String> colNames) {
    Column[] newCols = new Column[colNames.size()];
    int j = 0;
    for (String name : colNames) {
      newCols[j++] = getColumnByName(name);
    }
    return new Table(newCols, index);
  }

  public Table join(Table other) {
    int s = (int) nrows();
    int[] matches = new int[s];
    for (int i = 0; i < s; i++) {
      matches[i] = other.index.loc(index.iloc(i));
    }
    Column[] newColumns = new Column[this.columns.length + other.columns.length];
    System.arraycopy(columns, 0, newColumns, 0, columns.length);
    for (int i = 0; i < other.columns.length; i++) {
      Column original = other.columns[i];
      newColumns[i + other.columns.length] =
          new Column(original.getName(), index, original.getStorage().orderMask(matches));
    }
    return new Table(newColumns, index);
  }
}
